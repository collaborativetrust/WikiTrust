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

(** This is the revision object used for the on-line author reputation and text trust evaluation *)
class revision 
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

      (* Things to store: 
	 tot_edit_qual
	 n_edit_judges (these two allow the computation of the edit longevity)
	 tot_rep (total earned reputation )
         A list of revisions and authors to which the current revision gave a 
	 reputation increase, so that the same increase can be undone 
	 total_text  
	 n_text_judges

       *)
      (* These have to do with the revision text.  There is nothing in this, 
	 since we do not necessarily read the text of all the revisions we 
	 may want to keep track: reading the text is expensive. *)
    val mutable words : word array option = None
    val mutable trust : float array option = None
    val mutable origin : int array option = None
    val mutable seps  : Text.sep_t array option = None 
    val mutable sep_word_idx : int array option = None 

    (* This is a Vec of previous revisions by the same author *)
    val mutable prev_by_author : 'a Vec.t = Vec.empty 

    (* These quantities keep track of the quality of a revision *)

    (* Edit *)
    (* Number of revisions that act as edit judges of the current one *)
    val mutable n_edit_judges : int  = n_edit_judges_init 
    (* Total edit quality of the revision *)
    val mutable total_edit_quality : float = total_edit_quality_init
    (* Minimum edit quality the revision has received so far *)
    val mutable min_edit_quality : float = min_edit_quality_init 

    (* Text *)
    (* Number of revisions that act as text judges of the current one *)
    val mutable n_text_judges : int  = n_text_judges_init 
    (* Amount of text created in the revision *)
    val mutable new_text : int = new_text_init 
    (* Total amount of left-over text *)
    val mutable persistent_text : int = persistent_text_init 

    (* Basic access methods *)
    method get_id : int = id
    method get_ip : string = if user_id = 0 then username else ""
    method get_time : float = time
    method get_page_id : int = page_id
    method get_user_id : int = user_id
    method get_user_name : string = username 
    method get_is_anon : bool = is_anon

      (* Reads the revision text from the db, and splits it appropriately *)
    method read_text : unit = 
      let text_vec = Vec.singleton (db#read_colored_markup rev_id) in 
      let (w, t, o, s_idx, s) = Text.split_into_words_seps_and_info in 
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
	  | None -> Raise ReadTextError
	end

    method get_trust : word array =
      match trust with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match trust with 
	    Some w -> w
	  | None -> Raise ReadTextError
	end

    method get_origin : word array =
      match origin with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match origin with 
	    Some w -> w
	  | None -> Raise ReadTextError
	end

    method get_seps : Text.sep_t array = 
      match seps with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match seps with 
	    Some w -> w
	  | None -> Raise ReadTextError
	end

    method get_sep_word_idx : Text.sep_t array = 
      match sep_word_idx with 
	Some w -> w
      | None -> begin 
	  self#read_text;
	  match sep_word_idx with 
	    Some w -> w
	  | None -> Raise ReadTextError
	end

    (** This method adds to the revision immediately preceding revisions by the same author. *)
    method add_by_same_author (r: 'a) : unit = 
      prev_by_author <- Vec.append r prev_by_author

    (** This method returns the immediately preceding revision by the same author, if any *)
    method get_preceding_by_same_author : 'a option = 
      if Vec.length prev_by_author = 0 
      then None
      else Some (Vec.get 0 prev_by_author)

  end (* revision object *)
