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

type word = string;;
exception ReadTextError


(** This is the revision object used for the on-line author reputation and text trust evaluation *)
class revision 
  (db: Online_db.db)
  (rev_id: int) (* revision id *)
  (page_id: int) (* page id *)
  (time: float) (* time, as a floating point *)
  (user_id: int) (* user id *)
  (username: string) (* name of the user *)
  (is_minor: bool) 
  (comment: string)
  =
  object (self : 'a)
    val is_anon : bool = (user_id = 0)
      (* These have to do with the revision text.  There is nothing in this, 
	 since we do not necessarily read the text of all the revisions we 
	 may want to keep track: reading the text is expensive. *)
    val mutable words : word array option = None
    val mutable trust : float array option = None
    val mutable origin : int array option = None
    val mutable seps  : Text.sep_t array option = None 
    val mutable sep_word_idx : int array option = None 

    (* These quantities keep track of the quality of a revision *)
    (* First, I have a flag that says whether I have read the quantities 
       or not.  They are not read by default; they reside in a 
       separate table from standard revision data. *)
    val mutable quality_info_opt: qual_info_t option = None 
    (* Dirty bit to avoid writing back unchanged stuff *)
    val mutable modified_quality_info : bool = false

    (* Basic access methods *)
    method get_id : int = rev_id
    method get_ip : string = if user_id = 0 then username else ""
    method get_time : float = time
    method get_page_id : int = page_id
    method get_user_id : int = user_id
    method get_user_name : string = username 
    method get_is_anon : bool = is_anon

      (* Reads the revision text from the db, and splits it appropriately *)
    method read_text : unit = 
      let text_vec = Vec.singleton (db#read_colored_markup rev_id) in 
      let (w, t, o, s_idx, s) = Text.split_into_words_seps_and_info text_vec in 
      words <- Some w; 
      trust <- Some t; 
      origin <- Some o; 
      seps <- Some s; 
      sep_word_idx <- Some s_idx

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
	  self#read_text;
	  match trust with 
	    Some w -> w
	  | None -> raise ReadTextError
	end

    method get_origin : int array =
      match origin with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match origin with 
	    Some w -> w
	  | None -> raise ReadTextError
	end

    method get_seps : Text.sep_t array = 
      match seps with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match seps with 
	    Some w -> w
	  | None -> raise ReadTextError
	end

    method get_sep_word_idx : int array = 
      match sep_word_idx with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match sep_word_idx with 
	    Some w -> w
	  | None -> raise ReadTextError
	end

    (** Reads the quality info from the database *)
    method private read_quality_info : qual_info_t = 
      match quality_info_opt with 
	None -> begin 
	  let q = match db#read_quality_info rev_id with 
	      Some qq -> qq
	    | None -> quality_info_default
	  in 
	  quality_info_opt <- Some q;
	  modified_quality_info <- false;
	  q
	end
      | Some q -> q

    (** Writes quality info to the database *)
    method write_quality_info : unit = 
      if modified_quality_info then begin
	match quality_info_opt with 
	  None -> ()
	| Some qual_info -> begin 
	    db#write_quality_info rev_id qual_info;
	    modified_quality_info <- false;
	  end
      end

    (** Adds edit quality information *)
    method add_edit_quality_info (new_q: float) : unit = 
      let q = self#read_quality_info in 
      q.total_edit_quality <- q.total_edit_quality +. new_q; 
      if new_q < q.min_edit_quality then q.min_edit_quality <- new_q; 
      q.n_edit_judges <- q.n_edit_judges + 1; 
      modified_quality_info <- true

    method get_nix : bool = 
      let q = self#read_quality_info in 
      q.nix_bit

    method set_nix_bit : unit = 
      let q = self#read_quality_info in 
      q.nix_bit <- true;
      modified_quality_info <- true


  end (* revision object *)

let make_revision row db: revision = 
  let set_is_minor ism = match ism with
    | 0 -> false
    | 1 -> true
    | _ -> assert false in
  new revision db 
    (not_null int2ml row.(0)) 
    (not_null int2ml row.(1))                 
    (float_of_string (not_null str2ml row.(2))) 
    (not_null int2ml row.(3)) 
    (not_null str2ml row.(4)) 
    (set_is_minor (not_null int2ml row.(5)))                           
    (not_null str2ml row.(6))
