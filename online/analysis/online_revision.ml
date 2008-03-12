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

    (* This counts how many revisions ago a revision was made, 
       counting consecutive revisions by the same author as one revision only. *)
    val mutable age : int = 0 

    (* These quantities keep track of the quality of a revision *)
    (* First, I have a flag that says whether I have read the quantities 
       or not.  They are not read by default; they reside in a 
       separate table from standard revision data. *)
    val mutable has_quality_info : bool = false
    (* Dirty bit to avoid writing back unchanged stuff *)
    val mutable modified_quality_info : bool = false

    (* Edit *)
    (* Number of revisions that act as edit judges of the current one *)
    val mutable n_edit_judges : int  = 0
    (* Total edit quality of the revision *)
    val mutable total_edit_quality : float = 0.
    (* Minimum edit quality the revision has received so far *)
    val mutable min_edit_quality : float = 0.

    (* Text *)
    (* Number of revisions that act as text judges of the current one *)
    val mutable n_text_judges : int  = 0
    (* Amount of text created in the revision *)
    val mutable new_text : int = 0
    (* Total amount of left-over text *)
    val mutable persistent_text : int = 0

    (* Basic access methods *)
    method get_id : int = id
    method get_ip : string = if user_id = 0 then username else ""
    method get_time : float = time
    method get_page_id : int = page_id
    method get_user_id : int = user_id
    method get_user_name : string = username 
    method get_is_anon : bool = is_anon
    method set_age (n: int) : unit = age <- n 
    method get_age : int = age

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

    (** Reads the quality info from the database *)
    method read_quality_info : unit = 
      if not has_quality_info then begin 
	let (n_ed_j, t_ed_q, m_ed_q, n_tx_j, new_tx, tot_tx) = db#read_quality_info rev_id in 
	n_edit_judges <- n_ed_j; 
	total_edit_quality <- t_ed_q; 
	min_edit_quality <- m_ed_q; 
	n_text_judges <- n_tx_j; 
	new_text <- new_tx; 
	persistent_text <- tot_tx;
	has_quality_info <- true;
	modified_quality_info <- false
      end

    (** Writes quality info to the database *)
    method write_quality_info : unit = 
      if has_quality_info && modified_quality_info then begin 
	db#write_quality_info rev_id n_edit_judges total_edit_quality min_edit_quality
	  n_text_judges new_text persistent_text; 
	modified_quality_info <- false
      end

    (** Adds edit quality information *)
    method add_edit_quality_info (q: float) : unit = 
      if not has_quality_info then self#read_quality_info; 
      total_edit_quality <- total_edit_quality +. q; 
      if q < min_edit_quality then min_edit_quality <- q; 
      n_edit_judges <- n_edit_judges + 1; 
      modified_quality_info <- true

    (** Adds text quality information *)
    method add_text_quality_info (q: int) : unit = 
      if not has_quality_info then self#read_quality_info; 
      persistent_text <- persistent_text + q; 
      n_text_judges <- n_text_judges + 1; 
      modified_quality_info <- true

    (** Sets the amount of new text found in a revision *)
    method set_new_text (q: int) : unit = 
      if not has_quality_info then self#read_quality_info; 
      new_text <- q; 
      modified_quality_info <- true

    (** Returns the amount of new text found in a revision *)
    method get_new_text : int = 
      if not has_quality_info then self#read_quality_info; 
      new_text

    (** Returns the amount of new text in this revision, as well as in all
	consecutive previous revisions by the same author *)
    method get_all_text_by_same_consecutive_author : int = 
      let f x y = x#get_new_text + y in 
      Vec.fold f 0 prev_by_author 
	

  end (* revision object *)
