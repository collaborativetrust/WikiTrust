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
	between versions i (source, left) and j (dest, right) of the 
	revisions in revs. *)
    val edit_list : ((int, int), Editlist.edit) Hashtbl.t
    (** These are the edit distances, indexed as above. *)
    val edit_dist : ((int, int), int) Hashtbl.t
    (** This is the revision of text.ml used to split strings *)
    val mutable text_split_version : string = "" *)

    (** [init_revs n] initializes an array consisting of [n] past revisions, 
	if they can be found in the db, or less. *)
    method init_revs (n: int) : unit = 
      let db_p = new Db_page.page page_id in 
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
      (* gets text etc of last version *) 
      let rev2_t = rev2#get_words in 
      let rev2_l = Array.length (rev2_t) in 
      let rev2_i = Chdiff.make_index_diff rev2_t in 
      (* Now it determines the edit list of rev2 from those at rev2_idx +1, ...+2,
	 and so forth. *)
      for rev1_idx = rev2_idx + 1 to last_idx do begin 
        let rev1 = Vec.get rev1_idx revs in 
        let rev1_t = rev1#get_words in 
        let rev1_l = Array.length (rev1_t) in 

        (* Decides which method to use: zipping the lists, or 
           computing the precise distance.  
           If rev1 is the revision before rev2, there is no choice *)
        if rev1_idx - 1 = rev2_idx then begin 
          let edits  = Chdiff.edit_diff rev1_t rev2_t rev2_i in 
          let d      = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	  Hashtbl.add edit_list (rev1_idx, rev2_idx) edits;
	  Hashtbl.add edit_dist (rev1_idx, rev2_idx) d
        end else begin 
          (* We will choose the intermediary which gives the best coverage *)
          let best_middle_idx = ref (-1) in 
          (* for best_coverage, the smaller, the better: measures uncovered amount *)
          let best_coverage   = ref (rev1_l + rev2_l + 1) in 
          for revm_idx = rev2_idx + 1 to rev1_idx - 1 do begin 
            let revm = Vec.get revm_idx revs in 
            let revm_e = Hashtbl.find edit_list (revm_idx, rev2_idx) in 
	    let forw_e = Hashtbl.find edit_list (rev1_idx, revm_idx) in 
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
            let revm_e = Hashtbl.find edit_list (revm_idx, rev2_idx) in 
	    let forw_e = Hashtbl.find edit_list (rev1_idx, revm_idx) in 
            (* computes the distance via zipping. *)
            let edits = Compute_edlist.edit_diff_using_zipped_edits rev1_t rev2_t forw_e revm_e in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	    Hashtbl.add edit_list (rev1_idx, rev2_idx) edits;
	    Hashtbl.add edit_dist (rev1_idx, rev2_idx) d
          end else begin 
            (* Nothing suitable found, uses the brute-force approach of computing 
	       the edit distance from direct text comparison. ¯*)
            let edits   = Chdiff.edit_diff rev1_t rev2_t rev2_i in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
	    Hashtbl.add edit_list (rev1_idx, rev2_idx) edits;
	    Hashtbl.add edit_dist (rev1_idx, rev2_idx) d
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
	    for j = i + 1 to n_revs - 1 do begin 
	      let rev_j = Vec.get j revs in 
	      let revid_j = rev_j#get_id in 
	      let edl = db#read_edit_diff revid_j revid_i in 
	      Hashtbl.add edit_list (j, i) edl
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
	    db#write_edit_diff revid_j revid_0 (Hashtbl.find edit_list (j, 0))
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
	      db#write_edit_diff revid_j revid_i (Hashtbl.find edit_list (j, i)) 
	    end done
	  end done;
	  (* Now they are up to date *)
	  db#write_text_split_version page_id current_version

	end (* Recomputation of all edit lists *)
      end (* If there is only one revision *)


