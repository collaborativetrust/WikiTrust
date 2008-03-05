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
  (page_id: int) = 
  
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
    val edit_list : ((int * int), (int * int * Editlist.edit)) Hashtbl.t
    (** These are the edit distances, indexed as above. *)
    val edit_dist : ((int * int), float) Hashtbl.t
    (** This is the revision of text.ml used to split strings *)
    val mutable text_split_version : string = "" *)

    (** [init_revs n] initializes an array consisting of [n] past revisions, 
	if they can be found in the db, or less. *)
    method init_revs (n: int) : unit = 
      let db_p = new Db_page.page db page_id in 
      let i = ref n in 
      while !i > 0 do begin 
	match db_p#get_latest_rev with 
	  None -> i := 0 
	| Some r -> begin 
	    Vec.append r revs;
	    i := !i - 1
	  end
      end


    (** This private method computes the edit list between revision last_rev_idx
	and all previous revisions in revs, storign the results in edit_list and edit_dist. *)
    method (* private *) compute_edit_lists_to_rev (last_rev_idx: int) : unit = 
      (* Index of last revision *)
      let last_idx = (Vec.length revs) - 1 in 
      (* For uniform nicknames *)
      let rev2_idx = last_rev_idx in 
      let rev2 = Vec.get rev2_idx revs in 
      let rev2_id = rev2#get_id in 
      (* gets text etc of last version *) 
      let rev2_t = rev2#get_words in 
      let rev2_l = Array.length (rev2_t) in 
      let rev2_i = Chdiff.make_index_diff rev2_t in 
      (* Now it determines the edit list of rev2 from those at rev2_idx +1, ...+2,
	 and so forth. *)
      for rev1_idx = rev2_idx + 1 to last_idx do begin 
        let rev1 = Vec.get rev1_idx revs in
	let rev1_id = rev1#get_id in 
        let rev1_t = rev1#get_words in 
        let rev1_l = Array.length (rev1_t) in 

        (* Decides which method to use: zipping the lists, or 
           computing the precise distance.  
           If rev1 is the revision before rev2, there is no choice *)
        if rev1_idx - 1 = rev2_idx then begin 
          let edits  = Chdiff.edit_diff rev1_t rev2_t rev2_i in 
          let d      = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	  Hashtbl.add edit_list (rev1_id, rev2_id) (rev1_l, rev2_l, edits);
	  Hashtbl.add edit_dist (rev1_id, rev2_id) d
        end else begin 
          (* We will choose the intermediary which gives the best coverage *)
          let best_middle_idx = ref (-1) in 
          (* for best_coverage, the smaller, the better: measures uncovered amount *)
          let best_coverage   = ref (rev1_l + rev2_l + 1) in 
          for revm_idx = rev2_idx + 1 to rev1_idx - 1 do begin 
            let revm = Vec.get revm_idx revs in 
	    let revm_id = revm#get_id in 
            let (_, _, revm_e) = Hashtbl.find edit_list (revm_id, rev2_id) in 
	    let (_, _, forw_e) = Hashtbl.find edit_list (rev1_id, revm_id) in 
	    (* Computes a zipped edit list from rev1 to rev2 *)
            let zip_e = Compute_edlist.zip_edit_lists revm_e forw_e in 
            let (c1, c2) = Compute_edlist.diff_cover zip_e in 
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
            let (_, _, revm_e) = Hashtbl.find edit_list (revm_id, rev2_id) in 
	    let (_, _, forw_e) = Hashtbl.find edit_list (rev1_id, revm_id) in 
            (* computes the distance via zipping. *)
            let edits = Compute_edlist.edit_diff_using_zipped_edits rev1_t rev2_t forw_e revm_e in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	    Hashtbl.add edit_list (rev1_id, rev2_id) (rev1_l, rev2_l, edits);
	    Hashtbl.add edit_dist (rev1_id, rev2_id) d
          end else begin 
            (* Nothing suitable found, uses the brute-force approach of computing 
	       the edit distance from direct text comparison. ¯*)
            let edits   = Chdiff.edit_diff rev1_t rev2_t rev2_i in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	    Hashtbl.add edit_list (rev1_id, rev2_id) (rev1_l, rev2_l, edits);
	    Hashtbl.add edit_dist (rev1_id, rev2_id) d
          end
        end (* Tries to use zipping. *)
      end (* for rev1_idx: for all previous revisions in the array *)


    (** This method computes all the revision-to-revision edit lists and distances among the
	revisions.  It assumes that the revision 0 is the newest one, and it has not been 
	compared to any existing revision. *)
    method compute_edit_list : unit = 
      let n_revs = Vec.length revs in 
      (* If there is only one revision, there is nothing to do. *)
      if n_revs > 1 then begin 
	(* Existing edit lists may be 
	   generated with an older version of the code that splits revisions
	   into words, so we need to check them, and in case, rebuild them. 
	   Gets the current version of the code. *)
	let current_version = Text.version in 
	let used_version = db#read_text_split_version page_id in 
	(* If the current version of text splitting is equal to the old one, 
	   then we just need to do the distances from the most recent page. *)
	if current_version = used_version then begin 

	  (* Reads the edit lists between revisions that already exist, as they still apply. *)
	  for i = 1 to n_revs - 2 do begin 
	    let rev_i = Vec.get i revs in 
	    let revid_i = rev_i#get_id in 
	    let rev_i_l = Array.length (rev_i#get_words) in 
	    for j = i + 1 to n_revs - 1 do begin 
	      let rev_j = Vec.get j revs in 
	      let revid_j = rev_j#get_id in 
	      let edl = db#read_edit_diff revid_j revid_i in 
	      let rev_j_l = Array.length (rev_j#get_words) in 
	      let d = Editlist.edit_distance edl (max rev_i_l rev_j_l) in 
	      Hashtbl.add edit_list (revid_j, revid_i) (rev_i_l, rev_j_l, edl); 
	      Hashtbl.add edit_dist (revid_j, revid_i) d
	    end done
	  end done; 
	  (* This method computes the edit list for position k of the table, 
	     with k=0 in this call, given that the edit lists between all 
	     higher-numbered entried in revs have already been correctly 
	     determined. *)
	  self#compute_edit_lists_to_rev 0; 
	  (* Now we must write to the db the newly computed edit lists *)
	  let rev_0 = Vec.get 0 revs in
	  let revid_0 = rev_0#get_id in 
	  for j = 1 to n_revs - 1 do begin 
	    let rev_j = Vec.get j revs in 
	    let revid_j = rev_j#get_id in 
	    let (_, _, el) = Hashtbl.find edit_list (revid_j, revid_0) in 
	    db#write_edit_diff revid_j revid_0 el 
	  end done

	end else begin 
	  (* We need to recompute all edit lists *)
	  for i = n_revs - 2 downto 0 do self#compute_edit_lists_to_rev i done; 
	  (* And we write them all to disk *)
	  for i = nrevs - 2 downto 0 do begin 
	    let rev_i = Vec.get i revs in 
	    let revid_i = rev_i#get_id in 
	    for j = nrevs - 1 downto i + 1 do begin 
	      let rev_j = Vec.get j revs in 
	      let revid_j = rev_j#get_id in 
	      let el = Hashtbl.find edit_list (revid_j, revid_i) in 
	      db#write_edit_diff revid_j revid_i el
	    end done
	  end done;
	  (* Now they are up to date *)
	  db#write_text_split_version page_id current_version

	end (* Recomputation of all edit lists *)
      end (* If there is only one revision *)


    (** Gets a list of dead chunks coming from the disk, and translates them into 
	arrays, leaving position 0 free for the current revision. *)
    method (* private *) chunks_to_array (chunk_l: chunk_t list) :
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


    (** This method computes the trust of the revision 0, given that the edit distances from 
	previous revisions are known. 
	The method is as follows: it compares the newest revision 0 with both the preceding one, 
	1, and with the closest to 0 in edit distance, if different from 1. 
	The trust is then computed as the maximum of the two figures. *)
    method compute_trust : unit = 
      let n_revs = Vec.length revs in 
      let rev = Vec.get 0 revs in 
      let rev_id = rev#get_id in 
      let uid = rev#get_user_id in 
      let rev_t = rev#get_words in 
      let rev_l = Array.length rev_t in 
      let rev_seps = rev#get_seps in 
      (* Gets the author reputation from the db *)
      let rep_float = Rep.weight (db#get_rep uid) in 
      (* Now we proceed by cases, depending on whether this is the first revision of the 
	 page, or whether there have been revisions before. *)
      if n_revs = 1 then begin 

	(* It's the first revision.  Then all trust is simply inherited from the 
	   author's reputation.  The revision trust will consist of only one chunk, 
	   with the trust as computed. *)
	let new_text_trust = rep_float *. trust_coeff_lends_rep in 
	let chunk_0_trust = Array.make rev_l new_text_trust in 
	let chunk_0_origin = Array.make rev_l rev_id in 
	(* Produces the live chunk, consisting of the text of the revision, annotated
	   with trust and origin information *)
	let chunk_0 = Revision.produce_annotated_markup rev_seps chunk_0_trust chunk_0_origin true true in 
	(* And writes it out to the db *)
	db#write_colored_markup rev_id chunk0; 
	db#write_dead_page_chunks page_id []

      end else begin 

	(* There is at least one past revision. *)

	(* Gets the data for the latest revision, to then do trust computation. *)
	let rev0 = Vec.get 0 revs in 
	let rev1 = Vec.get 1 revs in 
	let rev0_id = rev0#get_id in 
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

	if !close_idx = 1 then begin 

	  (* The most recent revision was most likely obtained by editing the immediately 
	     preceding revision.  Computes word trust as usual. *)
	  let (_, _, medit_l) = Hashtbl.find edit_list (rev1_id, rev0_id) in 
	  (* Builds list of chunks of previous revision for comparison *)
	  chunks_a.(0) <- rev1#get_words; 
	  trust_a.(0)  <- rev1#get_trust;
	  origin_a.(0) <- rev1#get_origin;
	  (* Calls the function that analyzes the difference 
             between revisions rev1_id --> rev0_id. Data relative to the previous revision
             is stored in the instance fields chunks_a *)
	  let (new_chunks_a, medit_l) = Chdiff.text_tracking chunks_a new_wl in 
	  (* Calls the function that computes the trust of the newest revision. *)
	  (* If the author is the same, we do not increase the reputation of exisiting text, 
	     to thwart a trivial attack. *)
	  let (c_read_all, c_read_part) = 
	    if rev0#get_uid = rev1#get_uid 
	    then (0., 0.) 
	    else (trust_coeff_read_all, trust_coeff_read_part)
	  in 
	  let new_trust_a = Compute_trust.compute_trust_chunks 
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
	  (* Computes the origin of the new text *)
	  let new_origin_a = Compute_trust.compute_origin origin_a new_chunks_a medit_l rev0_id in 
	  
	  (* ---qui--- I have to: 
	     - select the deleted chunks to write to disk, and write them 
	     - write the new colored revision 
	   *)
	  


	end else begin 
	  (* The most recent revision was most likely obtained by editing a revision k that 
	     precedes the immediately preceding one. 
	     Computes the trust that would result from these two edits: 1 -> 0, and k -> 0, 
	     and assigns to each word the maximum trust that any of these two methods compute. 
	     Also takes care of setting the deleted chuncks correctly (see comments later). *)



	end (* which version is closer to the current one *)
      end (* There is at least one past revision *)



	
	
	
