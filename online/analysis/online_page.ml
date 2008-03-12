(*

Copyright (c) 2008 The Regents of the University of California
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

open Eval_constants;;
open Online_types;;

(** This is a class representing a page, or article, at a high level. 
    Most of the functions implementing an online edit are implemented 
    in this file. *)

class page 
  (db: Online_db.db) 
  (logger: Online_log.logger)
  (page_id: int) 
  (revid_to_analyze: int) = 
  
  object (self) 
    (** This is the Vec of existing revisions for the page *)
    val revs: Online_revision.revision Vec.t = Vec.empty 
    (** These are the edit lists.  The position (i, j) is the edit list 
	between revision id i (source, left) and revision id j (dest, right) of the 
	revisions in revs. 
        The content of the hash table contains an entry (n, m, el), where:
        - n is the length of the left text
        - m is the length of the right text
        - el is the edit list. *)
    val edit_list : ((int * int), (int * int * Editlist.edit)) Hashtbl.t = Hashtbl.create 10
    (** These are the edit distances, indexed as above. *)
    val edit_dist : ((int * int), float) Hashtbl.t = Hashtbl.create 10
    (** This is a hashtable from revision id to revision, for the revisions that 
	have been read from disc. *)
    val revid_to_rev: (int, Online_revision.revision) Hashtbl.t = Hashtbl.create Online_constants.n_revs_to_consider 

    (** This is a hashtable from revision ids, to reputaton increment to the authors
	of that revision.  This is used to write the feedback produced by this
	revision; it is not used to update the user reputations themselves. 
	The next hash table is used for that purpose. *)
    val revid_to_rep_inc : (int, float) Hashtbl.t = Hashtbl.create 10
      (** This is a hashtable from user ids, to reputation increments to the 
	  authors, and is used to collect all reputation changes, so that 
	  they can be done all at the end. *)
    val uid_to_rep_inc : (int, float) Hashtbl.t = Hashtbl.create 10

    (** [read_revs n] initializes an array consisting of [n] past revisions, 
	if they can be found in the db, or less. 
	The newest revision is the latest in the db; for this revision, we also know the
	deleted chunks. 
	After this revision, we keep only the latest among consecuive revisions by the same author.
     *)
    method private read_revs : unit = 
      let db_p = new Db_page.page db page_id in 
      (* Puts the first revision first *)
      match db_p#get_latest_rev with 
	None -> Vec.empty 
      | Some r -> begin 

	  (* Reads the most recent revision *)
	  let rv = ref (Vec.singleton r) in 
	  r#set_age 0; 
	  let uid = r#get_user_id in 
	  let rid = r#get_id in 
	  Hashtbl.add revid_to_uid rid (uid, r#get_user_name, 0, r#get_time); 

	  (* Reads some previous ones.
	     Note that the immediately preceding revision has a special 
	     status, since we need it in distance computations, so we make 
	     sure that we consider it, even though it may be from the same 
	     author as the most recent one. *)
	  let is_preceding = ref true in 

	  let i = ref Online_constants.n_revs_to_read in 
	  let prev_by_auth = ref r in 
	  let age = ref 0 in 
	  (* !i is the number of revision blocks by different authors that we must read. 
	     Assume the revisions are r1 r2 r3 r4 r5 r6 r7 r8 r9 
	     by authors               u1 u1 u2 u2 u2 u3 u4 u4 u5
	     and !i = 3.  
	     Then, we read them to r3 included, so that we have the full contribution by u2. *)
	  while !i > 0 do begin 
	    match db_p#get_latest_rev with
	      None -> i := 0 
	    | Some r -> begin 
		let uid = r#get_user_id in 
		let rid = r#get_id in 
		Hashtbl.add revid_to_rev rid r
		(* If the author is different from the current one *)
		if (Revision.different_author equate_anons r !prev_by_auth)
		  || !is_preceding then begin 
		    is_preceding := false
		    i := !i - 1;
		    if !i > 0 then begin 
		      age := !age + 1; 
		      r#set_age !age; 
		      rv := Vec.append r !rv;
		      prev_by_auth := r;
		    end
		  end else begin 
		    (* same author as before *)
		    r#set_age !age; 
		    !prev_by_auth#add_by_same_author r; 
		  end
	      end
	  end; 
	  !rv
	end


    (** Keeps track of the reputation increments to users. *)
    method private inc_author_rep (uid: int) (q: float) : unit = 
      if Hashtbl.mem uid_to_rep_inc uid then begin 
	let tot = Hashtbl.find uid_to_rep_inc uid in 
	Hashtbl.replace uid_to_rep_inc uid (tot +. q)
      end else begin 
	Hashtbl.add uid_to_rep_inc uid q
      end


    (** Keeps track of the reputation increments to revisions. *)
    method private inc_rev_rep (rev_id: int) (q: float) : unit = 
      if Hashtbl.mem revid_to_rep_inc rev_id then begin 
	let tot = Hashtbl.find revid_to_rep_inc rev_id in 
	Hashtbl.replace revid_to_rep_inc rev_id (tot +. q)
      end else begin 
	Hashtbl.add revid_to_rep_inc rev_id q
      end


   (** General method for increasing reputations *)
    method private inc_reputation (rev_id: int) (uid: int) (q: float) : unit = 
      self#inc_author_rep uid q; 
      self#inc_rev_rep rev_id q


   (** Writes the reputation increments to disc. 
       This method must be called at the very end. *)
    method private write_reputation_increments : unit = 
      let rev0 = Vec.get 0 revs in 
      let rev0_id = rev0#get_id in 
      let rev0_uid = rev0#get_user_id in 
      let rev0_time = rev0#get_time in 
      (* Goes over the revisions one by one, and writes the edges
	 in the reputation transfer graph. *)
      let f (rev_id: int) (q: float) = 
	begin 
	  let rev = Hashtbl.find revid_to_rev rev_id in 
	  let uid = rev#get_user_id in 
	  db#write_feedback rev0_id rev0_uid rev_id uid rev0_time q false; 
	end 
      in 
      Hashtbl.iter f revid_to_rep_inc;
      (* Writes the changes in user reputations *)
      let g (uid: int) (q: float) = 
	let old_rep = db#get_rep uid in 
	let new_rep = old_rep + q in 
	db#set_rep uid new_rep
      in 
      Hashtbl.iter g uid_to_rep_inc


    (** [undo_reputation_update r] undoes the reputation updates done by a 
	revision [r] to authors.  This is used to roll back [r] effects if 
	[r] is not the last among consecutve versions by the same author.  *)
    method private undo_reputation_update (r: Online_revision.revision) : unit = 
      let rev_id = r#get_id in 
      let rev_uid = r#get_user_id in 
      (* Reads from the db the list of changes *)
      let l = db#read_feedback_by rev_id in 
      (* Now iterates over the list l, and changes the user reputations *)
      let f (el : (int * int * float * float * bool)) : unit = 
	let (rev2_id, rev2_uid, timestamp, q, reverted) = el in 
	self#inc_author_rep rev2_uid (-. q); 
	db#write_feedback rev_id rev_uid rev2_id rev2_uid timestamp q true 
      in 
      List.iter f l; 


    (** This method computes all the revision-to-revision edit lists and distances among the
	revisions.  It assumes that the revision 0 is the newest one, and it has not been 
	compared to any existing revision. *)
    method private compute_edit_lists : unit =       
      let n_revs = Vec.length revs in 
      (* If there is only one revision, there is nothing to do. *)
      if n_revs > 1 then begin 
	(* Existing edit lists may be 
	   generated with an older version of the code that splits revisions
	   into words, so we need to check them, and in case, rebuild them. 
	   Gets the current version of the code. *)
	let current_version = Text.version in 

	(* Now reads or computes all the triangle distances.
	   It starts from the end of the Vec, and progresses towards the beginning. *)
	let last_idx = n_revs - 1 in 
	for rev1_idx = last_idx - 1 downto 0 do begin 
	  let rev1 = Vec.get rev1_idx revs in 
	  let rev1_t = rev1#get_words in 
	  let rev1_l = Array.length rev1_t in 
	  let rev1_id = rev1#get_id in 
	  let rev1_i = Chdiff.make_index_diff rev1_t in 
	  (* We now must read or compute the distance between rev1_idx and all previous
	     revisions.  I iterate with rev_2_idx that goes from most recent, to oldest, 
	     as it is easier and better to compute distances over shorer time and revision
	     spans than longer ones. *)
	  for rev2_idx = rev1_idx + 1 to last_idx do begin 
	    let rev2 = Vec.get rev2_idx revs in 
	    let rev2_t = rev2#get_words in 
	    let rev2_l = Array.length rev2_t in 
	    let rev2_id = rev2#get_id in 

	    (* The easiest case is when the distance can be read from the database. *)
	    let edl_opt = db#read_edit_diff rev2_id rev1_id in 
	    (* This needs to be recomputed if it cannot be found, or if it is computed 
	       with an old method. *)
	    let must_recompute = 
	      match edl_opt with 
		None -> true
	      | Some (vers, edl) -> vers != current_version 
	    in 
	    if must_recompute then begin 
	      (* The edit list must be computed. *)
	      let (edl, d) = 
		(* Decides which method to use: zipping the lists, or 
		   computing the precise distance.  
		   If rev1 is the revision before rev2, there is no choice *)
		if rev2_idx = rev1_idx + 1 then begin 
		  let edits  = Chdiff.edit_diff rev2_t rev1_t rev1_i in 
		  let d      = Editlist.edit_distance edits (max rev1_l rev2_l) in 
		  (edits, d)
		end else begin 
		  (* We will choose the intermediary which gives the best coverage *)
		  let best_middle_idx = ref (-1) in 
		  (* for best_coverage, the smaller, the better: measures uncovered amount *)
		  let best_coverage   = ref (rev1_l + rev2_l + 1) in 
		  for revm_idx = rev1_idx + 1 to rev2_idx - 1 do begin 
		    let revm = Vec.get revm_idx revs in 
		    let revm_id = revm#get_id in
		    (* We have that: 
		       edit_list (revm_id, rev1_id) is defined, as it was computed in a previous 
		                                    iteration of for rev2_idx 
		       edit_list (rev2_id, revm_id) is defined, as it was computed in a previous 
		                                    iteration of for rev1_idx 
		       So any Not_found here indicate an algorithm error. *)
		    let (_, _, rev1_e) = Hashtbl.find edit_list (revm_id, rev1_id) in 
		    let (_, _, rev2_e) = Hashtbl.find edit_list (rev2_id, revm_id) in 
		    (* Computes a zipped edit list from rev1 to rev2 *)
		    let zip_e = Compute_edlist.zip_edit_lists rev2_e rev1_e in 
		    let (c2, c1) = Compute_edlist.diff_cover zip_e in 
		    (* Computes the amount of uncovered text *)
		    let unc1 = rev1_l - c1 in 
		    let unc2 = rev2_l - c2 in 
		    let unc = min unc1 unc2 in 
		    (* Computes the percentages of uncovered *)
		    let perc1 = (float_of_int (unc1 + 1)) /. (float_of_int (rev1_l + 1)) in 
		    let perc2 = (float_of_int (unc2 + 1)) /. (float_of_int (rev2_l + 1)) in 
		    let perc  = min perc1 perc2 in 
		    (* If it qualifies, and if it is better than the best, use it *)
		    if perc <= max_perc_to_zip && unc <= max_uncovered_to_zip && unc < !best_coverage then begin 
		      best_coverage := unc; 
		      best_middle_idx := revm_idx
		    end
		  end done; 
		  if !best_middle_idx > -1 then begin 
		    (* It has found some suitable zipping; uses it. *)
		    let revm = Vec.get !best_middle_idx revs in 
		    let (_, _, rev1_e) = Hashtbl.find edit_list (revm_id, rev1_id) in 
		    let (_, _, rev2_e) = Hashtbl.find edit_list (rev2_id, revm_id) in 
		    (* computes the distance via zipping. *)
		    let edits = Compute_edlist.edit_diff_using_zipped_edits rev2_t rev1_t rev2_e rev1_e in 
		    let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
		    (edits, d)
		  end else begin 
		    (* Nothing suitable found, uses the brute-force approach of computing 
		       the edit distance from direct text comparison. ¯*)
		    let edits   = Chdiff.edit_diff rev2_t rev1_t rev1_i in 
		    let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
		    (edits, d)
		  end
		end (* Tries to use zipping. *)
	      in 
	      (* Writes it to the hash table *)
	      Hashtbl.add edit_list (rev2_id, rev1_id) (rev2_l, rev1_l, edl)
	      let d = Editlist.edit_distance edl (max rev2_l rev1_l) in 
	      Hashtbl.add edit_dist (rev2_id, rev1_id) d; 
	      (* and writes it to disk *)
	      db#write_edit_diff rev2_id rev1_id current_version edl 
	    end else begin 
	      (* The edit list can be found on disk *)
	      match edl_opt with 
		None -> ()
	      | Some (vers, edl) -> begin 
		  Hashtbl.add edit_list (rev2_id, rev1_id) (rev2_l, rev1_l, edl);
		  let d = Editlist.edit_distance edl (max rev2_l rev1_l) in 
		  Hashtbl.add edit_dist (rev2_id, rev1_id) d
		end
	    end

	  end (* for rev2_idx *)
	end (* for rev1_idx *)
      end (* if more than one revision *)


    (** Gets a list of dead chunks coming from the disk, and translates them into 
	arrays, leaving position 0 free for the current revision. *)
    method private chunks_to_array (chunk_l: chunk_t list) :
      (word array array, float array array, int array array, int array, float array) = 
      let n_els = 1 + List.length chunk_l in 
      let chunks_a = Array.make n_els (Array.make 0 "") in 
      let trust_a  = Array.make n_els (Array.make 0 0.) in 
      let origin_a = Array.make n_els (Array.make 0 0)  in 
      let age_a    = Array.make n_els 0  in 
      let time_a   = Array.make n_els 0. in 
      (* Fills the arrays one by one *)
      let i = ref 0 in 
      (* This function is iterated on the list *)
      let f (c: chunk_t) : unit = begin
	i := !i + 1; 
	chunks_a.(!i) <- c.text; 
	trust_a.(!i)  <- c.trust; 
	origin_a.(!i) <- c.origin; 
	age_a.(!i)    <- c.n_del_revisions; 
	time_a.(!i)   <- c.timestamp
      end in 
      List.iter f chunk_l; 
      (chunks_a, trust_a, origin_a, age_a, time_a)


    (** This method computes the list of dead chunks, updating appropriately their age and 
	count, and selects the ones that are not too old. 
	[compute_dead_chunk_list new_chunks_a new_trust_a new_origin_a original_chunk_l 
	previous_time current_time medit_l] computes
	[(live_chunk, live_trust, live_origin, chunk_l)]. 
        [previous_time] is the time of the revision preceding the one whose list of dead chunks is being computed; 
	[current_time] is the current time. *)
    method private compute_dead_chunk_list 
      (new_chunks_a: word array array)
      (new_trust_a: float array array) 
      (new_origin_a: int array array)
      (original_chunk_l: chunk_t list)
      (previous_time: float)
      (current_time: float)
      (medit_l: Editlist.medit list) 
	: chunk_t list = 
      (* First of all, makes a Vec of chunk_t, and copies there the information. *)
      let chunk_v = ref Vec.empty in 
      for i = 1 to Array.length (new_chunks_a) - 1 do begin 
	let c = {
	  timestamp = 0.; (* we will fix this later *)
	  n_del_revisions = 0; (* we will fix this later *)
	  text = new_chunks_a.(i); 
	  trust = new_trust_a.(i); 
	  origin = new_origin_a.(i); 
	} in 
	chunk_v := Vec.append c !chunk_v
      end done; 
      let original_chunk_a = Array.of_list original_chunk_l in 
      (* Then, uses medit_l to update the age and timestamp information *)
      (* This function f will be iterated on the medit_l *)
      let f = function 
	  Mins (_, _) -> ()
	| Mdel (_, _, _) -> ()
	| Mmov (_, src_idx, _, dst_idx, _) -> begin 
	    (* if the destination is a dead chunk *)
	    if dst_idx > 0 then begin 
	      (* The (dst_idx - 1) is because in the destination, the live chunk is not 
		 present.  Similarly for the source. *)
	      let c = Vec.get (dst_idx - 1) chunk_v in 
	      (* Was the source a live chunk? *)
	      if src_idx = 0 then begin 
		(* yes, it was live *)
		c.timestamp <- previous_time; 
		c.n_del_revisions <- 1; 
	      end else begin 
		(* no it was dead *)
		c.timestamp <- original_chunk_a.(src_idx - 1).timestamp; 
		c.n_del_revisions <- original_chunk_a.(src_idx - 1).n_del_revisions + 1
	      end
	    end (* if destination was a dead chunk *)
	  end (* Mmov case *)
      in (* def of f *)
      List.iter f medit_l; 
      (* Ok, now the fields of chunk_v are set correctly. 
	 It filters chunk_v, removing old versions. *)
      (* The function p is the filter *)
      let p (c: chunk_t) : bool = 
	(current_time - c.timestamp < max_age_del_chunk)
	||
	(c.n_del_revisions < max_n_revs_del_chunk) 
      in 
      let chunk_v' = Vec.filter p chunk_v in 
      (* Finally, makes a list of these chunks *)
      Vec.to_list chunk_v'


    (** [compute_trust weight_user] computes the trust and origin of the text,
	and returns the origin of the live chunk: this origin information can 
	be used to update user reputations. *)
    method private compute_trust (weight_user: float) : int array = 
      let n_revs = Vec.length revs in 
      let rev0 = Vec.get 0 revs in 
      let rev0_id = rev#get_id in 
      let rev0_uid = rev#get_user_id in 
      let rev0_t = rev#get_words in 
      let rev0_l = Array.length rev_t in 
      let rev0_seps = rev#get_seps in 
      (* Gets the author reputation from the db *)
      let rep_float = Rep.weight (db#get_rep uid) in 
      (* Now we proceed by cases, depending on whether this is the first revision of the 
	 page, or whether there have been revisions before. *)
      if n_revs = 1 then begin 

	(* It's the first revision.  Then all trust is simply inherited from the 
	   author's reputation.  The revision trust will consist of only one chunk, 
	   with the trust as computed. *)
	let new_text_trust = rep_float *. trust_coeff_lends_rep in 
	let chunk_0_trust = Array.make rev0_l new_text_trust in 
	let chunk_0_origin = Array.make rev0_l rev0_id in 
	(* Produces the live chunk, consisting of the text of the revision, annotated
	   with trust and origin information *)
	let chunk_0 = Revision.produce_annotated_markup rev0_seps chunk_0_trust chunk_0_origin true true in 
	(* And writes it out to the db *)
	db#write_colored_markup rev0_id chunk0; 
	db#write_dead_page_chunks page_id [];

	(* Returns the origin of chunk 0, even though in this case, it 
	   won't cause any reputation update. *)
	chunk_0_origin

      end else begin 

	(* There is at least one past revision. *)

	let rev1 = Vec.get 1 revs in 
	let rev1_id = rev1#get_id in 
	let new_wl = rev0#get_words in 
	(* Reads from disk the deleted chunks list from the page *)
	let del_chunks_list = db#read_dead_page_chunks page_id in 
	(* And makes the arrays of deleted chunks of words, trust, and origin, 
	   leaving position 0 free, for the live page. *)
	let (chunks_a, trust_a, origin_a, age_a, timestamp_a) = self#chunks_to_array del_chunks_list in 

	(* I check whether the closest revision to the latest one is 
	   (a) the previous revision, or
	   (b) one of the revisions even before (indicating a reversion, 
	       essentially). *)
	let d_prev = Hashtbl.find edit_dist (rev1_id, rev0_id) in 
	let close_idx = ref 1 in 
	let closest_d = ref d_prev in 
	for i = 2 to n_revs do begin 
	  let revi = Vec.get i revs in 
	  let revi_id = revi#get_id in 
	  let d = Hashtbl.find edit_dist (revi_id, rev0_id) in  
	  (* We consider a revision to be a better candidate than the immediately preceding 
	     revision as the source of the most recent revision if it is less than 3 times 
	     closer than the current one. *) 
	  if d < d_prev /. 3. && d < closest_d then begin
	    close_idx := i; 
	    closest_d := d
	  end
	end done; 

	(* Compute the trust due to the change from revision idx 1 --> 0: 
	   we have to do this in any case. *)
	(* Builds list of chunks of previous revision for comparison *)
	chunks_a.(0) <- rev1#get_words; 
	trust_a.(0)  <- rev1#get_trust;
	(* Calls the function that analyzes the difference 
           between revisions rev1_id --> rev0_id. Data relative to the previous revision
           is stored in the instance fields chunks_a *)
	let (new_chunks_10_a, medit_10_l) = Chdiff.text_tracking chunks_a new_wl in 
	(* Calls the function that computes the trust of the newest revision. *)
	(* If the author is the same, we do not increase the reputation of exisiting text, 
	   to thwart a trivial attack. *)
	let (c_read_all, c_read_part) = 
	  if rev0#get_user_id = rev1#get_user_id 
	  then (0., 0.) 
	  else (trust_coeff_read_all, trust_coeff_read_part)
	in 
	let new_trust_10_a = Compute_trust.compute_trust_chunks 
	  trust_a 
	  new_chunks_a 
	  rev0#get_seps 
	  medit_l 
	  rep_float 
	  trust_coeff_lends_rep 
	  trust_coeff_kill_decrease 
	  trust_coeff_cut_rep_radius 
	  c_read_all
	  c_read_part
	  local_decay_coeff 
	in 

 	if !close_idx > 1 then begin 
	  (* The most recent revision was most likely obtained by editing a revision k that 
	     precedes the immediately preceding one. 
	     Computes the trust that would result from that edit, 
	     and assigns to each word the maximum trust that either this edit, or the edit 1 --> 0, 
	     would have computed. *)
	  let rev2 = Vec.get !close_idx revs in 
	  chunks_a.(0) <- rev2#get_words; 
	  trust_a.(0)  <- rev2#get_trust;
	  (* Calls the function that analyzes the difference 
             between revisions rev1_id --> rev0_id. Data relative to the previous revision
             is stored in the instance fields chunks_a *)
	  let (new_chunks_20_a, medit_20_l) = Chdiff.text_tracking chunks_a new_wl in 
	  (* Calls the function that computes the trust of the newest revision. *)
	  (* If the author is the same, we do not increase the reputation of exisiting text, 
	     to thwart a trivial attack. *)
	  let (c_read_all, c_read_part) = 
	    if rev0#get_user_id = rev2#get_user_id 
	    then (0., 0.) 
	    else (trust_coeff_read_all, trust_coeff_read_part)
	  in 
	  let new_trust_20_a = Compute_trust.compute_trust_chunks 
	    trust_a 
	    new_chunks_a 
	    rev0#get_seps 
	    medit_l 
	    rep_float 
	    trust_coeff_lends_rep 
	    trust_coeff_kill_decrease 
	    trust_coeff_cut_rep_radius 
	    c_read_all
	    c_read_part
	    local_decay_coeff 
	  in
	  (* The trust of each word is the max of the trust under both edits *)
	  for i = 0 to Array.length (new_trust_10_a.(0)) do begin 
	    new_trust_10_a.(0).(i) <- max new_trust_10_a.(0).(i) new_trust_20_a.(0).(i)
	  end done
	end; (* The closest version was not the immediately preceding one. *)

	(* Computes the origin of the new text; for this, we use the immediately preceding revision. *)
	origin_a.(0) <- rev1#get_origin;
	let new_origin_10_a = Compute_trust.compute_origin origin_a new_chunks_10_a medit_l rev0_id in 
	(* Computes the list of deleted chunks with extended information (also age, timestamp), 
	   and the information for the live text *)
	let (live_chunk, live_trust, live_origin, chunk_l) = 
	  self#compute_dead_chunk_list new_chunks_10_a new_trust_10_a new_origin_10_a del_chunks_list medit_l 
	in 
	(* Writes the revision to disk *)
	let buf = Revision.produce_annotated_markup rev0#get_seps live_trust live_origin true true in 
	db#write_colored_markup rev0_id (Buffer.contents buf); 
	db#write_dead_page_chunks page_id chunk_l;
	(* Ok, there is nothing more here to do for trust. *)
	(* Returns the text origin of the live text, used by text processing *)
	new_origin_10_a.(0)
      end (* There is at least one past revision *)


    (** [compute_edit_inc weight_user]  computes the edit increments to reputation due to 
	the last revision.  These edit increments are computed on the 
        basis of all the revisions in the Vec revs: that is why the 
        pairwise distances in vec have been evaluated. 
        We use as input the reputation weight of the last user, who acts as the judge. *)
    method private compute_edit_inc (weight_user: float) : unit = 
      let n_revs = Vec.length revs in 
      (* The "triangle" of revisions is formed as follows: rev0 (oldest), rev1 (judged), rev2 (judge). *)
      let rev2_idx = n_revs - 1 in 
      let rev2 = Vec.get rev2_idx revs in 
      let rev2_id    = rev2#get_id in 
      let rev2_uid   = rev2#get_user_id in 
      let rev2_uname = rev2#get_user_name in 
      let rev2_time  = rev2#get_time in 
      for rev1_idx = rev2_idx - 1 downto 1 do begin 
	let rev1 = Vec.get rev1_idx revs in 
	let rev1_id    = rev1#get_id in 
	let rev1_uid   = rev1#get_user_id in 
	let rev1_uname = rev1#get_user_name in 
	let rev1_time  = rev1#get_time in 
	(* If the author of rev2 and rev1 are the same, no judgement occurs. *)
	if Revison.different_author equate_anons rev2 rev1 then begin 
	  for rev0_idx = rev1_idx - 1 downto 0 do begin 
	    let rev0 = Vec.get rev0_idx revs in 
	    let rev0_id    = rev0#get_id in 
	    let rev0_uid   = rev0#get_user_id in 
	    let rev0_uname = rev0#get_user_name in 
	    let rev0_time  = rev0#get_time in 
	    (* Here we start the work on edit distances and reputation. *)
	    let d01 = Hashtbl.find edit_dist (rev0_id, rev1_id) in 
	    let d12 = Hashtbl.find edit_dist (rev1_id, rev2_id) in 
	    let d02 = Hashtbl.find edit_dist (rev0_id, rev2_id) in 
	    (* First, for logging purposes, produces the Edit_inc line *)
	    let s = Printf.sprintf "\nEditInc %10.0f PageId: %d rev0: %d uid0: %d uname0: %S rev1: %d uid1: %d uname1: %S rev2: %d uid2: %d uname2: %S d01: %7.2f d02: %7.2f d12: %7.2f n01: %d n12: %d t01: %d t12: %d"
	      time2 page_id 
	      rev0_id rev0_uid rev0_uname 
	      rev1_id rev1_uid rev1_uname 
	      rev2_id rev2_uid rev2_uname 
	      d01 d02 d12
	      (rev1 - rev0) (rev2 - rev1)
	      (int_of_float (rev1_time -. rev0_time)) (int_of_float (rev2_time -. rev1_time)) 
	    in 
	    logger#log s; 
	    (* Computes the edit quality *)
	    let qual = (d02 -. d12) /. d01 in 
	    (* Adds the quality information to rev1's record *)
	    rev1#add_edit_quality_info qual; 
	    (* Computes the reputation increase for the author of rev1 *)
	    if rev1_uid != 0 then begin 
	      (* This code is lifted from computerep.ml 
		 This code should match, as computerep is used to do the parameter optimization. *)
	      let spec_q = min 1.0 ((Online_constants.edit_leniency *. d01 -. d02) /. d12) in 
              (* takes into account of delta and the length exponent *)
              let q0 = spec_q *. (d12 ** Online_constants.length_exponent) in 
              (* punish the people who do damage *)
              let q1 = if q < 0.0 then q0 *. Online_constants.punish_factor else q0 in 
              let increment = q1 *. weight_user *. (1.0 -. Online_constants.text_vs_edit_weight) in 
	      (* Stores the increment -- we give them out all at the end *)
	      self#inc_reputation rev1_id rev1_uid increment
	    end (* if the author of rev1 is not anonymous *)	    
	  end done (* for rev0_idx *)
	end (* if different authors *)
      end done (* for rev1_idx *)
	

    (** [compute_text_inc weight_user origin_a] updates the author reputation 
	on the basis of the information on text origin. 
	[weight_user] is the weight of the user's reputation, 
	and [origin_a] is an array containing the revision ids 
	where the words originated. *)
    method private compute_text_inc 
      (weight_user: float) 
      (origin_a: int array) : unit = 
      let rev1 = Vec.get 0 revs in 
      let rev1_id = rev1#get_id in 
      let rev1_time = rev1#get_time in 
      let rev1_uid = rev1#get_user_id in 
      let rev1_name = rev1#get_user_name in 
      (* First, computes how much text there is due to each revision *)
      (* The hash table rev_to_text_q associates each recent revision with the amount of text added *)
      let rev_to_text_q: (int, int) Hashtbl.t = Hashtbl.create 10 in 
      let add_one_word (r_id: int) = 
	if Hashtbl.mem rev_to_text_q r_id then begin 
	  let q = Hashtbl.find rev_to_text_q r_id in 
	  Hashtbl.replace rev_to_text_q r_id (q + 1)
	end else begin 
	  Hashtbl.add rev_to_text_q r_id 1 
	end
      in 
      (* Sets the total amount of text introduced in this very revision *)
      if Hashtbl.mem rev_to_text_q rev1_id then begin 
	let q = Hashtbl.find rev_to_text_q rev1_id in 
	rev1#set_new_text q
      end; 
      (* Now computes the reputation update of all revisions that have been read from the disk *)
      Array.iter add_one_word origin_a; 
      (* Gives reputation to users in proportion to the number of words added 
	 in the various revisions. 
	 This function f is iterated on the hashtable rev_to_text_q *)
      let f (rev0_id: int) (q: int) : unit = 
	(* Takes only into account recent revisions *)
	if Hashtbl.mem revid_to_rev r_id then begin 
	  let rev0 = Hashtbl.find revid_to_rev rev0_id in 
	  let rev0_uid = rev0#get_user_id in 
	  if rev0_uid != 0 && rev0_uid != rev1_uid then begin 
	    let rev0_uname = rev0#get_user_name in 
	    let rev0_age = rev0#get_age in 
	    let rev0_time = rev0#get_time in 
	    let rev0_new_text = rev0#get_new_text in 
	    (* Produces the log line *)
	    let s = Printf.sprintf "\nTextInc %10.0f PageId: %d rev0: %d uid0: %d uname0: %S rev1: %d uid1: %d uname1: %S text: %d left: %d n01: %d t01: %d"
	      rev1_time page_id 
	      rev0_id rev0_uid rev0_uname 
	      rev1_id rev1_uid rev1_uname 
	      rev0_new_text q rev0_age (int_of_float (rev1_time -. rev0_time))
	    in 
	    logger#log s; 
	    (* Computes the reputation increment *)
	    let ratio_live = (float_of_int q) /. (float_of_int rev0_new_text) in 
	    let merit = ratio_live *. ((float_of_int q) ** Online_constants.length_exponent) in 
	    let increment = merit *. weight_user *. Online_constants.text_vs_edit_weight in 
	    self#inc_reputation rev0_id rev0_uid increment; 
	    (* And stores the fact that there is one more text judge that found q *)
	    rev0#adds_text_quality_info q; 
	  end 
	end
      in Hashtbl.iter f rev_to_text_q


    (** This method computes the trust of the revision 0, given that the edit distances from 
	previous revisions are known. 
	The method is as follows: it compares the newest revision 0 with both the preceding one, 
	1, and with the closest to 0 in edit distance, if different from 1. 
	The trust is then computed as the maximum of the two figures. 
        The method computes also the effects on author reputation as a result of the new revision. *)
    method eval : unit = 
      (* First, reads the revisions *)
      self#read_revs; 
      let n_revs = Vec.length revs in 

      (* Computes the edit distances *)
      self#compute_edit_lists; 

      (* rev is the last revision, that needs evaluation *)
      let rev = Vec.get 0 revs in 
      let rev_id = rev#get_id in 
      let uid = rev#get_user_id in 
      let weight_user = log (1. +. (db#get_rep uid)) in 

      (* Computes, and writes to disk, the trust of the newest revision *)
      let chunk0_origin = self#compute_trust weight_user in 

      (* We now work on reputation. *)

      (* I want to take into account only the latest among consecutive revisions 
	 by the same author, when computing reputation increments.  
	 Thus, if the latest revision is by the same author as the previous one, 
	 we undo all the changes to author reputations done by the previous one, 
	 before computing the new reputation updates. *)
      if n_revs > 1 then begin 
	let rev1 = Vec.get 1 revs in 
	if not Revision.different_author equate_anons rev rev1 then 
	  self#undo_reputation_update rev1
      end; 

      (* We now process the reputation update. *)
      self#compute_text_inc weight_user rev_id chunk0_origin;
      if n_revs > 2 then self#compute_edit_inc weight_user;
      (* and we write them to disk *)
      self#write_reputation_increments; 
      
      (* Finally, we write back to disc the quality information of all revisions *)
      (* The function f is iterated on revid_to_rev *)
      let f (revid: int) (r: Online_revision.revision) : unit = 
	r#write_quality_info 
      in Hashtbl.iter f revid_to_rev; 
      
      (* Flushes the logger.  *)
      logger#flush;
      

  end (* class *)

