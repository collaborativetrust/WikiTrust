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
type word = string


(** This is a class representing a page, or article, at a high level. 
    Most of the functions implementing an online edit are implemented 
    in this file. See the mli file for the call parameters. *)

class page 
  (db: Online_db.db) 
  (logger: Online_log.logger)
  (page_id: int) 
  (revision_id: int) 
  (trust_coeff: trust_coeff_t) 

= 
  
  object (self) 
    (** This is the Vec of existing revisions for the page *)
    val mutable revs: Online_revision.revision Vec.t = Vec.empty 
    (** These are the edit lists.  The position (i, j) is the edit list 
        between revision id i (source, left) and revision id j (dest, right) of the 
        revisions in revs. 
        The content of the hash table contains an entry (n, m, el), where:
        - n is the length of the left text
        - m is the length of the right text
        - el is the edit list. *)
    val edit_list : ((int * int), (int * int * Editlist.edit list)) Hashtbl.t = Hashtbl.create 10
    (** These are the edit distances, indexed as above. *)
    val edit_dist : ((int * int), float) Hashtbl.t = Hashtbl.create 10
    (** This is a hashtable from revision id to revision, for the revisions that 
        have been read from disc.  This hash table contains all revisions, whether 
        it is the last in a block by the same author, or not. *)
    val revid_to_rev: (int, Online_revision.revision) Hashtbl.t = Hashtbl.create trust_coeff.n_revs_to_consider
    (** This is a hash table mapping each user id to a pair (old_rep, new_rep option). 
	So we can keep track of both the original value, and of the updated value, if
	any.  *)
    val rep_cache : (int, (float * float option)) Hashtbl.t = Hashtbl.create 10
    (** Current time *)
    val mutable curr_time = 0.

    (** This initializer reads the past revisions from the online database, 
        puts them into the revs Vec, and also produces the [revid_to_rev] 
        hash table.
     *)
    initializer 
      (* Reads the most recent revisions *)
      let db_p = new Db_page.page db page_id revision_id in 
      let i = ref trust_coeff.n_revs_to_consider in 
      while (!i > 0) do begin 
        match db_p#get_rev with
          None -> i := 0 (* We have read all revisions *)
        | Some r -> begin
	    let rid = r#get_id in 
            Hashtbl.add revid_to_rev rid r;
	    revs <- Vec.append r revs; 
	    i := !i - 1;
	  end (* Some r *)
      end done;
      (* Sets the current time *)
      let n_revs = Vec.length revs in 
      if n_revs > 0 then begin 
	let r = Vec.get 0 revs in 
	curr_time <- r#get_time
      end
 

    (** This method returns the current value of the user reputation *)
    method private get_rep (uid: int) : float = 
      if Hashtbl.mem rep_cache uid then begin 
	let (old_rep, new_rep_opt) = Hashtbl.find rep_cache uid in 
	match new_rep_opt with 
	  Some r -> r
	| None -> old_rep
      end else begin 
	(* We have to read it from disk *)
	let r = db#get_rep uid in 
	Hashtbl.add rep_cache uid (r, None); 
	r
      end

    (** This method sets, but does not write to disk, a new user reputation. *)
    method private set_rep (uid: int) (r: float) : unit = 
      if Hashtbl.mem rep_cache uid then begin 
	let (old_rep, _) = Hashtbl.find rep_cache uid in 
	Hashtbl.replace rep_cache uid (old_rep, Some r)
      end else begin 
	(* We have to read it from disk *)
	let old_rep = db#get_rep uid in 
	Hashtbl.add rep_cache uid (old_rep, Some r)
      end

    (** Write all new reputations to the db *)
    method private write_all_reps : unit = 
      let f uid = function 
	  (old_r, Some r) -> begin
	    (* Writes the new reputation *)
	    db#set_rep uid r;
	    (* Writes the reputation change in the history *)
	    db#set_rep_hist uid curr_time old_r r
	  end
	| (old_r, None) -> ()
      in Hashtbl.iter f rep_cache


    (** Computes the weight of a reputation *)
    method private weight (r: float) : float = log (1.1 +. r)


    (** This method computes all the revision-to-revision edit lists and distances among the
        revisions.  It assumes that the revision 0 is the newest one, and that it has not been 
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
		    let revm_id = revm#get_id in 
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
              Hashtbl.add edit_list (rev2_id, rev1_id) (rev2_l, rev1_l, edl);
              let d = Editlist.edit_distance edl (max rev2_l rev1_l) in 
              Hashtbl.add edit_dist (rev2_id, rev1_id) d; 
	      Hashtbl.add edit_dist (rev1_id, rev2_id) d;
              (* and writes it to disk *)
              db#write_edit_diff rev2_id rev1_id current_version edl 
            end else begin 
              (* The edit list can be found on disk *)
              match edl_opt with 
                None -> ()
              | Some (vers, edl) -> begin 
                  Hashtbl.add edit_list (rev2_id, rev1_id) (rev2_l, rev1_l, edl);
                  let d = Editlist.edit_distance edl (max rev2_l rev1_l) in 
                  Hashtbl.add edit_dist (rev2_id, rev1_id) d;
		  Hashtbl.add edit_dist (rev1_id, rev2_id) d
                end
            end

          end done (* for rev2_idx *)
        end done (* for rev1_idx *)
      end (* if more than one revision *)


    (** Gets a list of dead chunks coming from the disk, and translates them into 
        arrays, leaving position 0 free for the current revision. *)
    method private chunks_to_array (chunk_l: chunk_t list) :
      (word array array * float array array * int array array * int array * float array) = 
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
        medit_l previous_time current_time] computes
        [(live_chunk, live_trust, live_origin, chunk_l)]. 
        [previous_time] is the time of the revision preceding the one whose list of 
                        dead chunks is being computed; 
        [current_time] is the current time. *)
    method private compute_dead_chunk_list 
      (new_chunks_a: word array array)
      (new_trust_a: float array array) 
      (new_origin_a: int array array)
      (original_chunk_l: chunk_t list)
      (medit_l: Editlist.medit list) 
      (previous_time: float)
      (current_time: float)
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
          Editlist.Mins (_, _) -> ()
        | Editlist.Mdel (_, _, _) -> ()
        | Editlist.Mmov (_, src_idx, _, dst_idx, _) -> begin 
            (* if the destination is a dead chunk *)
            if dst_idx > 0 then begin 
              (* The (dst_idx - 1) is because in the destination, the live chunk is not 
                 present.  Similarly for the source. *)
              let c = Vec.get (dst_idx - 1) !chunk_v in 
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
        (current_time -. c.timestamp < trust_coeff.max_del_time_chunk)
        ||
        (c.n_del_revisions < trust_coeff.max_del_revs_chunk)
      in 
      let chunk_v' = Vec.filter p !chunk_v in 
      (* Finally, makes a list of these chunks *)
      Vec.to_list chunk_v'


    (** [compute_trust weight_user] computes the trust and origin of the text. *)
    method private compute_trust : unit = 
      let n_revs = Vec.length revs in 
      let rev0 = Vec.get 0 revs in 
      let rev0_id = rev0#get_id in
      let rev0_uid = rev0#get_user_id in 
      let rev0_t = rev0#get_words in 
      let rev0_time = rev0#get_time in 
      let rev0_l = Array.length rev0_t in 
      let rev0_seps = rev0#get_seps in 
      (* Gets the author reputation from the db *)
      let weight_user = self#weight (self#get_rep rev0_uid) in 

      (* Now we proceed by cases, depending on whether this is the first revision of the 
         page, or whether there have been revisions before. *)
      if n_revs = 1 then begin 

        (* It's the first revision.  Then all trust is simply inherited from the 
           author's reputation.  The revision trust will consist of only one chunk, 
           with the trust as computed. *)
        let new_text_trust = weight_user *. trust_coeff.lends_rep in 
        let chunk_0_trust = Array.make rev0_l new_text_trust in 
        let chunk_0_origin = Array.make rev0_l rev0_id in 
        (* Produces the live chunk, consisting of the text of the revision, annotated
           with trust and origin information *)
        let chunk_0 = Revision.produce_annotated_markup rev0_seps chunk_0_trust chunk_0_origin true true in 
        (* And writes it out to the db *)
        db#write_colored_markup rev0_id (Buffer.contents chunk_0); 
        db#write_dead_page_chunks page_id []

      end else begin 

        (* There is at least one past revision. *)

        let rev1 = Vec.get 1 revs in 
        let rev1_id = rev1#get_id in 
	let rev1_time = rev1#get_time in 
        (* Reads from disk the deleted chunks list from the page *)
        let del_chunks_list = db#read_dead_page_chunks page_id in 
        (* And makes the arrays of deleted chunks of words, trust, and origin, 
           leaving position 0 free, for the live page. *)
        let (chunks_a, trust_a, origin_a, age_a, timestamp_a) = self#chunks_to_array del_chunks_list in 
	(* For the origin, we always consider the immediately preceding revision. *)
        origin_a.(0) <- rev1#get_origin;

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
          if d < d_prev /. 3. && d < !closest_d then begin
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
        let (new_chunks_10_a, medit_10_l) = Chdiff.text_tracking chunks_a rev0_t in 
        (* Calls the function that computes the trust of the newest revision. *)
        (* If the author is the same, we do not increase the reputation of exisiting text, 
           to thwart a trivial attack. *)
        let (c_read_all, c_read_part) = 
          if rev0#get_user_id = rev1#get_user_id 
          then (0., 0.) 
          else (trust_coeff.read_all, trust_coeff.read_part)
        in 
        let new_trust_10_a = Compute_trust.compute_trust_chunks 
          trust_a 
          new_chunks_10_a 
          rev0#get_seps 
          medit_10_l
          weight_user 
          trust_coeff.lends_rep 
          trust_coeff.kill_decrease 
          trust_coeff.cut_rep_radius 
          c_read_all
          c_read_part
          trust_coeff.local_decay
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
             between revisions rev2_id --> rev0_id. Data relative to the previous revision
             is stored in the instance fields chunks_a *)
          let (new_chunks_20_a, medit_20_l) = Chdiff.text_tracking chunks_a rev0_t in 
          (* Calls the function that computes the trust of the newest revision. *)
          (* If the author is the same, we do not increase the reputation of exisiting text, 
             to thwart a trivial attack. *)
          let (c_read_all, c_read_part) = 
            if rev0#get_user_id = rev2#get_user_id 
            then (0., 0.) 
            else (trust_coeff.read_all, trust_coeff.read_part)
          in 
          let new_trust_20_a = Compute_trust.compute_trust_chunks 
            trust_a 
            new_chunks_20_a 
            rev0#get_seps 
            medit_20_l
            weight_user 
            trust_coeff.lends_rep 
            trust_coeff.kill_decrease 
            trust_coeff.cut_rep_radius 
            c_read_all
            c_read_part
            trust_coeff.local_decay
          in
          (* The trust of each word is the max of the trust under both edits *)
          for i = 0 to Array.length (new_trust_10_a.(0)) do begin 
            new_trust_10_a.(0).(i) <- max new_trust_10_a.(0).(i) new_trust_20_a.(0).(i)
          end done
        end; (* The closest version was not the immediately preceding one. *)

        (* Computes the origin of the new text; for this, we use the immediately preceding revision. *)
        let new_origin_10_a = Compute_trust.compute_origin origin_a new_chunks_10_a medit_10_l rev0_id in 
        (* Computes the list of deleted chunks with extended information (also age, timestamp), 
           and the information for the live text *)
        let dead_chunk_l = self#compute_dead_chunk_list 
	  new_chunks_10_a new_trust_10_a new_origin_10_a del_chunks_list medit_10_l rev1_time rev0_time
        in 
        (* Writes the revision to disk *)
        let buf = Revision.produce_annotated_markup rev0#get_seps new_trust_10_a.(0) 
	  new_origin_10_a.(0) true true in 
        db#write_colored_markup rev0_id (Buffer.contents buf); 
        db#write_dead_page_chunks page_id dead_chunk_l

      end (* method compute_trust *)


    (** [compute_edit_inc]  computes the edit increments to reputation due to 
        the last revision.  These edit increments are computed on the 
        basis of all the revisions in the Vec revs: that is why the 
        pairwise distances in vec have been evaluated. 
        We use as input the reputation weight of the last user, who acts as the judge. *)
    method private compute_edit_inc : unit = 
      (* The Qual function (see paper) *)
      let qual d01 d12 d02 = (d02 -. d12) /. d01 in 
 
      let n_revs = Vec.length revs in 
      (* The "triangle" of revisions is formed as follows: rev2 (newest, and judge); rev1 (judged),
	 revp (reference, immediately preceding rev1), rev0 (reference, before rev1).
         We do anything only if there are at least 3 revisions in total. *)
      if n_revs > 2 then begin 
        let rev2 = Vec.get 0 revs in 
        let rev2_id    = rev2#get_id in 
        let rev2_uid   = rev2#get_user_id in 
        let rev2_uname = rev2#get_user_name in
        let rev2_time  = rev2#get_time in 
	let rev2_rep   = self#get_rep rev2_uid in 
	let rev2_weight = self#weight rev2_rep in 
	(* rev1_idx goes to 1 before the last, since we should be able to compare it 
	   to something *)
	for rev1_idx = 1 to n_revs - 2 do begin 
          let rev1 = Vec.get rev1_idx revs in 
          let rev1_id    = rev1#get_id in 
          let rev1_uid   = rev1#get_user_id in 
          let rev1_uname = rev1#get_user_name in 
          let rev1_time  = rev1#get_time in 
	  let rev1_rep   = self#get_rep rev1_uid in 
	  let rev1_nix   = ref (rev1#get_nix) in 
	  (* We read the data for revp *)
	  let revp_idx = rev1_idx + 1 in 
          let revp = Vec.get revp_idx revs in 
          let revp_id    = revp#get_id in 
	  (* delta is the edit work from revp to rev1 *)
	  let delta = Hashtbl.find edit_dist (revp_id, rev1_id) in 
	  (* We read a few distances, to cut down on hashtable access *)
          let d12 = Hashtbl.find edit_dist (rev2_id, rev1_id) in 
	  let dp1 = Hashtbl.find edit_dist (rev1_id, revp_id) in
	  let dp2 = Hashtbl.find edit_dist (rev2_id, revp_id) in 
	  (* We compute the reputation due to a past rev0 *)
	  for rev0_idx = rev1_idx + 1 to n_revs - 1 do begin 
            let rev0 = Vec.get rev0_idx revs in 
            let rev0_id    = rev0#get_id in 
            let rev0_uid   = rev0#get_user_id in 
            let rev0_uname = rev0#get_user_name in 
            let rev0_time  = rev0#get_time in 
	    let rev0_rep   = self#get_rep rev0_uid in 
	    (* Reads the other distances from the hash table *)
            let d02 = Hashtbl.find edit_dist (rev2_id, rev0_id) in 
            let d01 = Hashtbl.find edit_dist (rev1_id, rev0_id) in 
	    (* computes the two qualities *)
	    let qual_012 = qual d01 d12 d02 in 
	    let qual_p12 = qual dp1 d12 dp2 in 
	    let min_qual = min qual_012 qual_p12 in 
	    (* Adds edit quality information, for statistical analysis *)
	    rev1#add_edit_quality_info min_qual; 
	    (* computes the nixing bit *)
	    if (not !rev1_nix) && (rev2_time -. rev0_time < trust_coeff.nix_interval) 
	      && ((qual_012 < 0.) || (rev0_idx = n_revs - 1)) then begin 
		rev1_nix := true;
		rev1#set_nix_bit
	      end;
	    (* Computes inc_local_global (see paper) *)
	    let inc_local_global = trust_coeff.rep_scaling *. delta *. min_qual *. rev2_weight in
	    (* Applies it according to algorithm LOCAL-GLOBAL *)
	    let new_rep = 
	      if (!rev1_nix || rev2_time -. rev0_time < trust_coeff.nix_interval) 
		&& inc_local_global > 0. then begin 
		  (* caps the reputation increment *)
		  let cap_rep = min rev2_rep rev0_rep in
		  let capped_rep = min cap_rep (rev1_rep +. inc_local_global) in 
		  max rev1_rep capped_rep
	      end else begin 
		(* uncapped reputation increment *)
		rev1_rep +. inc_local_global
	      end
	    in 
	    self#set_rep rev1_uid new_rep;

            (* For logging purposes, produces the Edit_inc line *)
            let s = Printf.sprintf "\nEditInc %10.0f PageId: %d Delta: %7.2f rev0: %d uid0: %d uname0: %S rev1: %d uid1: %d uname1: %S rev2: %d uid2: %d uname2: %S d01: %7.2f d02: %7.2f d12: %7.2f dp2: %7.2f n01: %d n12: %d t01: %d t12: %d"
	      (* time and page id *)
	      rev2_time page_id delta
	      (* revision and user ids *)
              rev0_id rev0_uid rev0_uname 
              rev1_id rev1_uid rev1_uname 
              rev2_id rev2_uid rev2_uname 
	      (* word distances *)
	      d01 d02 d12 dp2 
	      (* distances between revisions in n. of revisions *)
	      (rev1_idx - rev0_idx) rev1_idx
	      (* distances between revisions in seconds *)
	      (int_of_float (rev1_time -. rev0_time)) (int_of_float (rev2_time -. rev1_time))
            in 
            logger#log s

	  end done (* for rev0_idx *)
	end done (* for rev1_idx *)
      end (* method compute_edit_inc *)


    (** This method computes the trust of the revision 0, given that the edit distances from 
        previous revisions are known. 
        The method is as follows: it compares the newest revision 0 with both the preceding one, 
        1, and with the closest to 0 in edit distance, if different from 1. 
        The trust is then computed as the maximum of the two figures. 
        The method computes also the effects on author reputation as a result of the new revision. *)
    method eval : unit = 

      (* Computes the edit distances *)
      self#compute_edit_lists; 

      (* Computes, and writes to disk, the trust of the newest revision *)
      self#compute_trust;

      (* We now process the reputation update. *)
      self#compute_edit_inc;
      (* and we write them to disk *)
      self#write_all_reps;
      
      (* Finally, we write back to disc the quality information of all revisions *)
      (* The function f is iterated on revid_to_rev *)
      let f (revid: int) (r: Online_revision.revision) : unit = 
        r#write_quality_info 
      in Hashtbl.iter f revid_to_rev; 
      
      (* Flushes the logger.  *)
      logger#flush;
      

  end (* class *)

