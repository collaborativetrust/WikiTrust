(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. The names of the contributors may not be used to endorse or promote
products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

 *)

open Vec;;
open Online_types;;
open Mysql;;
open Eval_defs;;

type word = string;;
exception ReadTextError


(** This is the revision object used for the on-line author reputation and text trust evaluation *)
class revision 
  (db: Online_db.db)
  (rev_id: int) (* revision id *)
  (page_id: int) (* page id *)
  (text_id: int) (* text id, we need it to save a db access later on *)
  (time_string: string) (* time, as a string yyyymmddhhmmss *)
  (user_id: int) (* user id *)
  (username: string) (* name of the user *)
  (is_minor: bool) 
  (comment: string)
  =
  let time_init = Timeconv.time_string_to_float time_string in 

  object (self : 'a)
    val is_anon : bool = (user_id = 0)
      (* These have to do with the revision text.  There is nothing in this, 
         since we do not necessarily read the text of all the revisions we 
         may want to keep track: reading the text is expensive. *)
    val time = time_init
    val mutable words : word array option = None
    val mutable trust : float array option = None
    val mutable origin : int array option = None
    val mutable seps  : Text.sep_t array option = None 
    val mutable sep_word_idx : int array option = None 

    (* These quantities keep track of the quality of a revision *)
    (* First, I have a flag that says whether I have read the quantities 
       or not.  They are not read by default; they reside in a 
       separate table from standard revision data. *)
    (* NOTE: Every revision needs ITS OWN copy of the quality info, so I cannot
       just do quality_info = quality_info_default, or they would all share the
       same copy. *)
    val mutable quality_info : qual_info_t = {
      n_edit_judges = quality_info_default.n_edit_judges;
      total_edit_quality = quality_info_default.total_edit_quality;
      min_edit_quality = quality_info_default.min_edit_quality;
      nix_bit = quality_info_default.nix_bit;
      delta = quality_info_default.delta;
      reputation_gain = quality_info_default.reputation_gain;
    }

    (* Dirty bit to avoid writing back unchanged stuff *)
    val mutable modified_quality_info : bool = false

      (** This initializer reads from the DB the information with the revision 
          specifically needed by the wikitrust algorithms *)
    initializer
      try 
        quality_info <- db#read_revision_info rev_id
      with Online_db.DB_Not_Found -> ()

    (* Basic access methods *)
    method get_id : int = rev_id
    method get_ip : string = if user_id = 0 then username else ""
    method get_time : float = time
    method get_page_id : int = page_id
    method get_user_id : int = user_id
    method get_user_name : string = username 
    method get_is_anon : bool = is_anon

      (* Reads the colored revision text from the db, and splits it appropriately *)
    method private read_colored_text : unit = 
      (* If there is an error here, it is transmitted up, so that the 
         reader deals in appropriate ways with the lack of trust information *)
      let text_vec = Vec.singleton (db#read_colored_markup rev_id) in 
      let (w, t, o, s_idx, s) = Text.split_into_words_seps_and_info text_vec in 
      words <- Some w; 
      trust <- Some t; 
      origin <- Some o; 
      seps <- Some s; 
      sep_word_idx <- Some s_idx

      (* Reads the revision text from the db, and splits it appropriately *)
    method private read_text : unit = 
      try 
        let text_vec = Vec.singleton (db#read_rev_text text_id) in 
        let (w, t, o, s_idx, s) = Text.split_into_words_seps_and_info text_vec in 
        words <- Some w; 
        seps <- Some s; 
        sep_word_idx <- Some s_idx
      with Online_db.DB_Not_Found -> begin 
        (* If the text has not been found, it is considered null. *)
        words <- Some [| |];
        seps <- Some [| |];
        sep_word_idx <- Some [| |]
      end

    method get_words : word array =
      match words with 
        Some w -> w
      | None -> begin 
          self#read_text;
          match words with 
            Some w -> w
          | None -> raise ReadTextError
        end

    method get_trust : float array =
      match trust with 
        Some w -> w
      | None -> begin 
          self#read_colored_text;
          match trust with 
            Some w -> w
          | None -> raise ReadTextError
        end

    method get_origin : int array =
      match origin with 
        Some w -> w
      | None -> begin 
          self#read_colored_text;
          match origin with 
            Some w -> w
          | None -> raise ReadTextError
        end

    method get_seps : Text.sep_t array = 
      match seps with 
        Some w -> w
      | None -> begin 
          self#read_colored_text;
          match seps with 
            Some w -> w
          | None -> raise ReadTextError
        end

    method get_sep_word_idx : int array = 
      match sep_word_idx with 
        Some w -> w
      | None -> begin 
          self#read_colored_text;
          match sep_word_idx with 
            Some w -> w
          | None -> raise ReadTextError
        end

    (** Adds edit quality information *)
    method add_edit_quality_info (delta: float) (new_q: float) (rep_gain: float) : unit = 
      (* updated *)
      quality_info.delta <- delta;
      quality_info.total_edit_quality <- quality_info.total_edit_quality +. new_q; 
      if new_q < quality_info.min_edit_quality then quality_info.min_edit_quality <- new_q; 
      quality_info.n_edit_judges <- quality_info.n_edit_judges + 1; 
      quality_info.reputation_gain <- quality_info.reputation_gain +. rep_gain;
      (* flags the change *)
      modified_quality_info <- true

    method get_nix : bool = quality_info.nix_bit

    method set_nix_bit : unit = 
      quality_info.nix_bit <- true;
      modified_quality_info <- true 

    (** [write_to_db] writes all revision information to the db. *)
    method write_to_db : unit = db#write_revision_info rev_id quality_info

  end (* revision class *)

let make_revision row db: revision = 
  let set_is_minor ism = match ism with
    | 0 -> false
    | 1 -> true
    | _ -> assert false in
  new revision db 
    (not_null int2ml row.(0)) (* rev id *)
    (not_null int2ml row.(1)) (* page id *)        
    (not_null int2ml row.(2)) (* text id *)
    (not_null str2ml row.(3)) (* timestamp *)
    (not_null int2ml row.(4)) (* user id *)
    (not_null str2ml row.(5)) (* user name *)
    (set_is_minor (not_null int2ml row.(6))) (* is_minor *)
    (not_null str2ml row.(7)) (* comment *)

