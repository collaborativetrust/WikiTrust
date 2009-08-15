(*

Copyright (c) 2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Krishnendu Chatterjee

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

open Eval_defs;;
open Online_types;;
type word = string
type rev_t = Online_revision.revision 

(** [Missing_trust (page_id, rev)] is raised if:
    - in a vote, the trust information for a revision voted on 
      is missing.  The vote should then most likely be discarded.
    - in an edit, this should never happen, and would be an indication of a bug. *)
exception Missing_trust of rev_t
(** This exception signals an internal error. *)
exception Missing_work_revision

(* This flag causes debugging info to be printed *)
let debug = true

(** This class contains the methods for computing the trust, author, and 
    origin of text for a revision of a page. *)

class page 
  (db: Online_db.db) 
  (page_id: int) 
  (revision_id: int) 
  (work_revision_opt_init: rev_t option)
  (trust_coeff: trust_coeff_t) 
  (n_retries: int) 

= 

  object (self) 

    (** This is the revision on which the class is called to work. 
	For votes, this needs to be initialized (hence the use
	of a mutable field). *)
    val mutable work_revision_opt = work_revision_opt_init

    (** This is the Vec of revisions for the page to which the last
	revision (included) is compared, obtained by union of the above. *)
    val mutable revs: rev_t Vec.t = Vec.empty 
    (** This is the Vec of recent revisions *)
    val mutable recent_revs: rev_t Vec.t = Vec.empty 

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
    (** This is a hash table mapping each user id to a pair (old_rep, new_rep option). 
	So we can keep track of both the original value, and of the updated value, if
	any.  *)
    val rep_cache : (int, (float * float option)) Hashtbl.t = Hashtbl.create 10
    (** Current time *)
    val mutable curr_time = 0.
    (** List of deleted chunks *)
    val mutable del_chunks_list : Online_types.chunk_t list = []
    (** Page information *)
    val mutable page_info = {
      past_hi_rep_revs = page_info_default.past_hi_rep_revs; 
      past_hi_trust_revs = page_info_default.past_hi_trust_revs;
    }
    (** Information about the histogram *)
    val delta_hist = Array.make Eval_defs.max_rep_val 0.
    val mutable new_hi_median = 0.
    val mutable histogram_updated = false


    (** This method reads the revision voted on from the online database, 
        and puts it into [work_revision_opt]. *)
    method private read_page_revisions_vote : unit = 
      (* Reads the page information *)
      begin 
	try 
	  let (cl, pinfo) = db#read_page_info page_id in 
	  del_chunks_list <- cl; 
	  page_info <- pinfo
	with Online_db.DB_Not_Found -> ();
      end;
      (* Reads the revision voted on. *)
      begin 
	try
	  let r = Online_revision.read_wikitrust_revision db revision_id in 
	  r#read_words_trust_origin_sigs;
	  work_revision_opt <- Some r
	with Online_db.DB_Not_Found -> raise Missing_work_revision
      end

    (** This method reads the past revisions from the online database, and
        puts them into the revs Vec. *)
    method private read_page_revisions_edit : unit = 
      (* Reads the page information *)
      begin 
	try 
	  let (cl, pinfo) = db#read_page_info page_id in 
	  del_chunks_list <- cl; 
	  page_info <- pinfo
	with Online_db.DB_Not_Found -> ();
      end;
      (* Reads the most recent revisions *)
      
      (* This is a hashtable from revision id to revision, for the revisions that 
         have been read from disc.  This hash table contains all revisions, whether 
         it is the last in a block by the same author, or not. *)
      let revid_to_rev: (int, rev_t) Hashtbl.t = 
	Hashtbl.create trust_coeff.n_revs_to_consider in
      (* First, puts in the latest revision. *)
      let rev_to_analyze = begin
	match work_revision_opt with 
	  None -> raise Missing_work_revision
	| Some r -> begin
	    Hashtbl.add revid_to_rev r#get_id r;
	    recent_revs <- Vec.singleton r;
	    r
	  end
      end in
      (* This is a feed of colored revisions previous from the current one. *)
      let db_p = new Db_page.page db rev_to_analyze trust_coeff.n_revs_to_consider in 
      let i = ref (trust_coeff.n_revs_to_consider - 1) in 
      while (!i > 0) do begin 
        match db_p#get_rev with
          None -> i := 0 (* We have read all revisions *)
        | Some r -> begin
	    let rid = r#get_id in 
	    Hashtbl.add revid_to_rev rid r;
	    recent_revs <- Vec.append r recent_revs; 
	    revs <- Vec.append r revs; 
	    i := !i - 1;
	  end (* Some r *)
      end done;
            
    (* [find_revision id] reads the revision from the hash table, 
       if possible, and otherwise from disk.  If it fails, it 
       still tries to carry on. *)
      let find_revision (id: int) : rev_t option = 
	if Hashtbl.mem revid_to_rev id 
	then Some (Hashtbl.find revid_to_rev id)
	else begin 
	  (* We need to read it. *)
	  try 
	    let r = Online_revision.read_wikitrust_revision db id in
	    Hashtbl.add revid_to_rev id r; 
	    Some r
	  with Online_db.DB_Not_Found -> None
	end
      in 

      (* Builds the Vec of high trust and high reputation revisions *)
      (* The function f is folded on a list of (revision ids * _) pairs, 
	 and produces a Vec of rev_t *)
      let f (u: rev_t Vec.t) el : rev_t Vec.t = 
	let (id, _) = el in
	match find_revision id with 
	  Some r -> Vec.append r u
	| None -> u
      in 
      let hi_trust_revs = List.fold_left f Vec.empty 
	page_info.past_hi_trust_revs in
      let f (u: rev_t Vec.t) el : rev_t Vec.t = 
	let (id, _, _) = el in
	match find_revision id with 
	  Some r -> Vec.append r u
	| None -> u
      in 
      let hi_rep_revs   = List.fold_left f Vec.empty 
	page_info.past_hi_rep_revs in
      
      (* This function merges two lists of revisions in chronological order *)
      let merge_chron (v1: rev_t Vec.t) (v2: rev_t Vec.t) : rev_t Vec.t = 
	let rec merge v w1 w2 = 
	  if w1 = Vec.empty then Vec.concat v w2
	  else if w2 = Vec.empty then Vec.concat v w1
	  else begin (* We must choose the least revision *)
	    let r1 = Vec.get 0 w1 in 
	    let r2 = Vec.get 0 w2 in 
	    let t1 = r1#get_time in 
	    let t2 = r2#get_time in 
	    let i1 = r1#get_id in 
	    let i2 = r2#get_id in 
	    if (t1, i1) = (t2, i2) then merge (Vec.append r1 v) (Vec.remove 0 w1) (Vec.remove 0 w2)
	    else begin 
	      if (t1, i1) > (t2, i2) 
	      then merge (Vec.append r1 v) (Vec.remove 0 w1) w2
	      else merge (Vec.append r2 v) w1 (Vec.remove 0 w2)
	    end
	  end
	in merge Vec.empty v1 v2
      in 
      
      (* Produces the Vec of all revisions *)
      revs <- merge_chron recent_revs (merge_chron hi_rep_revs hi_trust_revs); 
      let f (r: rev_t) : unit = !Online_log.online_logger#log (Printf.sprintf "%d " r#get_id) in 
      !Online_log.online_logger#log "\n  Recent   revisions: "; Vec.iter f recent_revs;
      !Online_log.online_logger#log "\n  Hi-trust revisions: "; Vec.iter f hi_trust_revs;
      !Online_log.online_logger#log "\n  Hi-rep   revisions: "; Vec.iter f hi_rep_revs;
      !Online_log.online_logger#log "\n  Total    revisions: "; Vec.iter f revs;
      
      (* Sets the current time *)
      let n_revs = Vec.length revs in 
      if n_revs > 0 then begin 
	let r = Vec.get 0 revs in 
	curr_time <- r#get_time
      end;
      
      (* Reads the revision text.  For the most recent revision, we
         read the normal text; for the others, the colored text *)
      for i = n_revs - 1 downto 0 do begin 
	let r = Vec.get i revs in
	if i = 0 then begin 
	  (* If a revision has no text, we have to recover this at a lower level,
	     since text is not something we compute. *)
	  r#read_text
	end else begin
	  try r#read_words_trust_origin_sigs
          with Online_db.DB_Not_Found -> raise (Missing_trust r)
	end
      end done


    (** High-m%-Median of an array *)
    method private compute_hi_median (a: float array) (m: float) =
      let total = Array.fold_left (+.) 0. a in 
      let mass_below = ref (total *. m) in 
      let median = ref 0. in 
      let i = ref 0 in 
      while (!mass_below > 0.) && (!i < max_rep_val) do begin 
	if a.(!i) > !mass_below then begin 
	  (* Median is in this column *)
	  median := !median +. !mass_below /. a.(!i);
	  mass_below := 0.; 
	end else begin 
	  (* Median is above this column *)
	  mass_below := !mass_below -. a.(!i); 
	  i := !i + 1;
	  median := !median +. 1. 
	end
      end done;
      !Online_log.online_logger#log (
	Printf.sprintf "\n %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %2.0f%%-median: %.2f" 
	  a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7) a.(8) a.(9) (m *. 100.) !median); 
      !median

    (** This method returns the current value of the user reputation *)
    method private get_rep (uid: int) : float = 
      if is_anonymous uid then 0. else begin 
	if Hashtbl.mem rep_cache uid then begin 
	  let (old_rep, new_rep_opt) = Hashtbl.find rep_cache uid in 
	  match new_rep_opt with 
	    Some r -> r
	  | None -> old_rep
	end else begin 
	  (* We have to read it from disk *)
	  let r = 
 	    try db#get_rep uid
	    with Online_db.DB_Not_Found -> 0.
	  in 
	  Hashtbl.add rep_cache uid (r, None); 
	  r
	end
      end

    (** This method sets, but does not write to disk, a new user reputation. *)
    method private set_rep (uid: int) (r: float) : unit = 
      if not_anonymous uid then begin 
	(* Reputations must be in the interval [0...maxrep] *)
	let r' = max 0. (min trust_coeff.max_rep r) in 
	(* Printf.printf "set_rep uid: %d r: %f\n" uid r'; debug *)
	if Hashtbl.mem rep_cache uid then begin 
	  let (old_rep, _) = Hashtbl.find rep_cache uid in 
	  Hashtbl.replace rep_cache uid (old_rep, Some r')
	end else begin 
	  (* We have to read it from disk *)
	  let old_rep = 
	    begin 
	      try db#get_rep uid
	      with Online_db.DB_Not_Found -> 0.
	    end
	  in 
	  Hashtbl.add rep_cache uid (old_rep, Some r')
	end
      end

    (** Write all new reputations to the db.  We write to the db reputation increments,
        so that we do not need to acquire locks: the writes from different 
        processes will merge without problems.
        TODO(Luca): Check that the use of a transaction is needed. *)
    method private write_all_reps : unit = 
      let f uid = function 
	  (old_r, Some r) ->  if abs_float (r -. old_r) > 0.001 then begin 
	    (* Writes the reputation change to disk, as a small transaction *)
	    let n_attempts = ref 0 in 
	    while !n_attempts < n_retries do 
	      begin 
		try begin 
		  db#start_transaction;
		  db#inc_rep uid (r -. old_r);
		  db#commit;
		  n_attempts := n_retries
		end with _ -> begin 
		  (* Roll back *)
		  db#rollback_transaction;
		  n_attempts := !n_attempts + 1
		end
	      end done (* End of the multiple attempts at the transaction *)
	  end
	| (old_r, None) -> ()
      in Hashtbl.iter f rep_cache


    (** Computes the weight of a reputation *)
    method private weight (r: float) : float = log (1.2 +. r)


    (** [insert_revision_in_lists] inserts the revision in the 
	list of high rep or high trust revisions if needed, and erases also 
	old signatures from revisions whose author signatures will no longer be 
	considered. *)
    method private insert_revision_in_lists : unit = 
      (* Gets info on the current revision. *)
      let cur_rev = match work_revision_opt with
	  None -> raise Missing_work_revision
	| Some r -> r 
      in
      let cur_trust = cur_rev#get_overall_trust in 
      let cur_id  = cur_rev#get_id in
      let cur_uid = cur_rev#get_user_id in 
      let cur_rep = self#get_rep cur_uid in 

      (* We keep track of which revisions we are throwing out, so that
         we can later remove the signatures. 
         If the list of recent revisions has grown to max size, we 
         propose to throw out its oldest element. 
         We will later check whether this last element belongs to one 
         of the other lists. *)
      let recent_ids = Vec.map (function r -> r#get_id) recent_revs in
      let (thrown_out, recent_not_thrown_out) = 
	let recent_l = Vec.length recent_revs in 
	if recent_l = trust_coeff.n_revs_to_consider 
	then 
	  (ref [Vec.get (recent_l - 1) recent_ids],
	  (Vec.remove (recent_l - 1) recent_ids))
	else (ref [], recent_ids)
      in 
      
      (* Trust first.  For trust, we keep the highest trust revisions so far. *)
      (* Finds the minimum of the trust for the revisions in the list, and their index *)
      page_info.past_hi_trust_revs <- 
	if page_info.past_hi_trust_revs = []
	then [(cur_id, cur_trust)]
	else begin
	  let hi_trust_revs = Vec.of_list page_info.past_hi_trust_revs in
	  (* Finds minimum trust in vector *)
	  let n = (Vec.length hi_trust_revs) - 1 in 
	  let min_trust_idx = ref n in 
	  let (_, t) = Vec.get n hi_trust_revs in
	  let min_trust = ref t in
	  for i = n - 1 downto 0 do begin 
	    let (_, t) = Vec.get i hi_trust_revs in 
	    if t < !min_trust then begin 
	      min_trust_idx := i;
	      min_trust := t
	    end
	  end done; 
	  (* Checks if minimum must be replaced *)
	  let (min_rev_id, _) = Vec.get !min_trust_idx hi_trust_revs in
	  if (cur_trust >= !min_trust) && (cur_id <> min_rev_id) 
	  then begin 
	    (* We insert the current revision *)
	    let shorter_list = 
	      if (Vec.length hi_trust_revs) = trust_coeff.len_hi_trust_revs then begin 
		(* We throw out the minimum revision if needed to keep list bounded *)
		thrown_out := min_rev_id :: !thrown_out; 
		Vec.remove !min_trust_idx hi_trust_revs
	      end else hi_trust_revs
	    in 
	    (cur_id, cur_trust) :: (Vec.to_list shorter_list)
	  end else begin
	    (* Unchanged *)
	    page_info.past_hi_trust_revs
	  end
	end;

      (* Now reputation.  For reputation, we keep the last revisions
	 above a high-reputation threshold. *)
      page_info.past_hi_rep_revs <- 
	if cur_rep > trust_coeff.hi_rep_list_threshold then begin 
	  let hi_rep_revs = Vec.of_list page_info.past_hi_rep_revs in
	  (* If the author is already in the list, kicks out the
	     previous revision by that author. *)
	  let shorter_list = 
	    match Vec.find (function (_, u, _) -> u = cur_uid) 0 hi_rep_revs with 
	      Some (i, (rid, uid, rep)) -> begin
		(* Yes, the same user id already was in the list.  Replace it. *)
		let ((out, _, _), shorter) = Vec.pop i hi_rep_revs in 
		thrown_out := out :: !thrown_out; 
		shorter
	      end
	    | None -> begin 
		(* No, the user_id was not in the list. *)
		let n = Vec.length hi_rep_revs in
		if n = trust_coeff.len_hi_rep_revs then begin 
		  let ((out, _, _), shorter) = Vec.pop (n - 1) hi_rep_revs in 
		  thrown_out := out :: !thrown_out; 
		  shorter
		end else hi_rep_revs
	      end
	  in
	  (cur_id, cur_uid, cur_rep) :: Vec.to_list shorter_list
	end else begin
	  (* not above threshold *)
	  page_info.past_hi_rep_revs
	end;

      (* We remove the signatures of the thrown out revisions, unless they appear 
	 in some list *)
      let is_in_some_list id = 
	(List.exists (function (i, _) -> i = id) page_info.past_hi_trust_revs)
	|| 
	  (List.exists (function (i, _, _) -> i = id) page_info.past_hi_rep_revs)
	||
	  (Vec.exists (function i -> i = id) recent_not_thrown_out)
      in
      let is_not_in_any_list id = not (is_in_some_list id) in
      let not_in_any_list = List.filter is_not_in_any_list !thrown_out in 
      let delete_sigs_of_rev id = db#delete_author_sigs page_id id in 
      List.iter delete_sigs_of_rev not_in_any_list


    (** This method computes all the revision-to-revision edit lists
        and distances among the revisions.  It assumes that the
        revision 0 is the newest one, and that it has not been
        compared to any existing revision. *)
    method private compute_edit_lists : unit =       
      let n_revs = Vec.length revs in 
      (* If there is only one revision, there is nothing to do. *)
      if n_revs > 1 then begin 
        (* Now reads or computes all the triangle distances.
	   It computes the distance between rev1 and all the previous revisions. 
	   We take rev1 to be the one-before-last revision, and we gradually progress
	   rev1 towards the most recent revision, that of index 0. 
	   This because, to compute the distance between rev1 and a previous revision rev2, 
	   we will often use information about intermediate revisions revm, so the 
	   chosen order of computation ensures that the distance between rev2 and revm is known. *)
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

	    (* Computes the edit list from rev2 to rev1 *)
	    let (edl, d) = 
              (* Decides which method to use: zipping the lists, or 
                 computing the precise distance.  
                 If rev2 is the revision before rev1, there is no choice *)
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
	    (* Writes the distance in the hash table *)
	    let d = Editlist.edit_distance edl (max rev2_l rev1_l) in 
	    Hashtbl.add edit_dist (rev2_id, rev1_id) d; 
	    Hashtbl.add edit_dist (rev1_id, rev2_id) d

          end done (* for rev2_idx *)
        end done (* for rev1_idx *)
      end (* if more than one revision *)


    (** Gets a list of dead chunks coming from the disk, and translates them into 
        arrays, leaving position 0 free for the current revision. *)
    method private chunks_to_array (chunk_l: chunk_t list) :
      (word array array *  (* chunks *)
	float array array *  (* trust *)
	Author_sig.packed_author_signature_t array array *  (* sigs *)
	int array array *  (* origin *)
	string array array *  (* author *)
	int array *  (* age *)
	float array  (* timestamp *)
      ) = 
      let n_els = 1 + List.length chunk_l in 
      let chunks_a = Array.make n_els [| |] in 
      let trust_a  = Array.make n_els [| |] in 
      let sigs_a   = Array.make n_els [| |] in 
      let origin_a = Array.make n_els [| |] in
      let author_a = Array.make n_els [| |] in
      let age_a    = Array.make n_els 0 in 
      let time_a   = Array.make n_els 0. in 
      (* Fills the arrays one by one *)
      let i = ref 0 in 
      (* This function is iterated on the list *)
      let f (c: chunk_t) : unit = begin
        i := !i + 1; 
        chunks_a.(!i) <- c.text; 
        trust_a.(!i)  <- c.trust;
	sigs_a.(!i)   <- c.sigs;
        origin_a.(!i) <- c.origin; 
	author_a.(!i) <- c.author;
        age_a.(!i)    <- c.n_del_revisions; 
        time_a.(!i)   <- c.timestamp
      end in 
      List.iter f chunk_l; 
      (chunks_a, trust_a, sigs_a, origin_a, author_a, age_a, time_a)


    (** This method computes the list of dead chunks, updating appropriately their age and 
        count, and selects the ones that are not too old. 
        [compute_dead_chunk_list new_chunks_a new_trust_a new_sigs_a 
	new_origin_a new_author_a original_chunk_l 
        medit_l previous_time current_time] computes
        [(live_chunk, live_trust, live_origin, chunk_l)]. 
        [previous_time] is the time of the revision preceding the one whose list of 
                        dead chunks is being computed; 
        [current_time] is the current time. *)
    method private compute_dead_chunk_list 
      (new_chunks_a: word array array)
      (new_trust_a: float array array) 
      (new_sigs_a: Author_sig.packed_author_signature_t array array) 
      (new_origin_a: int array array)
      (new_author_a: author_t array array)
      (original_chunk_l: chunk_t list)
      (medit_l: Editlist.medit list) 
      (previous_time: float)
      (current_time: float)
        : chunk_t list = 
      (* First of all, makes a Vec of chunk_t, and copies there the information. *)
      let chunk_v = ref Vec.empty in 
      for i = 1 to Array.length (new_chunks_a) - 1 do begin 
	(* If the chunk is too long, we truncate it. *)
	let c = 
	  if (Array.length new_chunks_a.(i)) > trust_coeff.max_dead_chunk_len
	  then {
            timestamp = 0.; (* we will fix this later *)
            n_del_revisions = 0; (* we will fix this later *)
            text   = Array.sub new_chunks_a.(i) 0 trust_coeff.max_dead_chunk_len; 
            trust  = Array.sub new_trust_a.(i)  0 trust_coeff.max_dead_chunk_len; 
            sigs   = Array.sub new_sigs_a.(i)   0 trust_coeff.max_dead_chunk_len; 
            origin = Array.sub new_origin_a.(i) 0 trust_coeff.max_dead_chunk_len; 
            author = Array.sub new_author_a.(i) 0 trust_coeff.max_dead_chunk_len; 
	  } else {
            timestamp = 0.; (* we will fix this later *)
            n_del_revisions = 0; (* we will fix this later *)
            text   = new_chunks_a.(i);
            trust  = new_trust_a.(i);
            sigs   = new_sigs_a.(i);
            origin = new_origin_a.(i);
            author = new_author_a.(i);
          }
	in chunk_v := Vec.append c !chunk_v
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


    (** [compute_trust] computes the trust and origin of the text. *)
    method private compute_trust : unit = 
      let n_revs = Vec.length revs in 
      let rev0 = Vec.get 0 revs in 
      let rev0_id = rev0#get_id in
      let rev0_uid = rev0#get_user_id in 
      let rev0_uname = rev0#get_user_name in 
      let rev0_t = rev0#get_words in 
      let rev0_time = rev0#get_time in 
      let rev0_time_string = rev0#get_time_string in 
      let rev0_l = Array.length rev0_t in 
      let rev0_seps = rev0#get_seps in 

      (* Gets the author reputation *)
      let rep_user = self#get_rep rev0_uid in 
      let weight_user = self#weight (rep_user) in 

      (* Now we proceed by cases, depending on whether this is the first revision of the 
         page, or whether there have been revisions before. *)
      if n_revs = 1 then begin 

        (* It's the first revision.  Then all trust is simply inherited from the 
           author's reputation.  The revision trust will consist of only one chunk, 
           with the trust as computed. *)
        let new_text_trust = weight_user *. trust_coeff.lends_rep in 
        let chunk_0_trust = Array.make rev0_l new_text_trust in 
        let chunk_0_origin = Array.make rev0_l rev0_id in 
        let chunk_0_author = Array.make rev0_l rev0_uname in 
	(* Produces the correct author signature; the function f is mapped on the 
	   array of words rev0_t *)
	let chunk_0_sigs = 
	  let f (w: string) : Author_sig.packed_author_signature_t = 
	      Author_sig.add_author rev0_uid w Author_sig.empty_sigs
	  in Array.map f rev0_t
	in 
        (* Produces the live chunk, consisting of the text of the revision, annotated
           with trust and origin information *)
        let buf = Revision.produce_annotated_markup 
	  rev0_seps chunk_0_trust chunk_0_origin chunk_0_author false true true in 
        (* And writes it out to the db *)
        db#write_colored_markup page_id rev0_id (Buffer.contents buf); 
	rev0#set_trust  chunk_0_trust;
	rev0#set_origin chunk_0_origin;
	rev0#set_author chunk_0_author;
	rev0#set_sigs   chunk_0_sigs;
	rev0#write_words_trust_origin_sigs;
	(* Computes the overall trust of the revision *)
	let t = Compute_robust_trust.compute_overall_trust chunk_0_trust in 
	rev0#set_overall_trust t

      end else begin 

        (* There is at least one past revision. *)

        let rev1 = Vec.get 1 revs in 
        let rev1_id = rev1#get_id in 
	let rev1_time = rev1#get_time in 
        (* Makes the arrays of deleted chunks of words, trust, and origin, 
           leaving position 0 free, for the live page. *)
        let (chunks_a, trust_a, sig_a, origin_a, author_a, age_a, timestamp_a) = 
	  self#chunks_to_array del_chunks_list in 
        (* I check whether the closest revision to the latest one is 
           (a) the previous revision, or
           (b) one of the revisions even before (indicating a reversion, 
               essentially). *)
        let d_prev = Hashtbl.find edit_dist (rev1_id, rev0_id) in 
        let close_idx = ref 1 in 
        let closest_d = ref d_prev in 
        for i = 2 to n_revs - 1 do begin 
          let revi = Vec.get i revs in 
          let revi_id = revi#get_id in 
          let d = Hashtbl.find edit_dist (revi_id, rev0_id) in  
          (* We consider a revision to be a better candidate than the
             immediately preceding revision as the source of the most
             recent revision if it is less than 3 times closer than
             the current one. *)
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
	sig_a.(0)    <- rev1#get_sigs;
        origin_a.(0) <- rev1#get_origin;
	author_a.(0) <- rev1#get_author;
        (* Calls the function that analyzes the difference 
           between revisions rev1_id --> rev0_id. Data relative to the previous revision
           is stored in the instance fields chunks_a *)
        let (new_chunks_10_a, medit_10_l) = Chdiff.text_tracking chunks_a rev0_t in 
	(* Computes origin *)
	let (new_origin_10_a, new_author_10_a) = 
	  Compute_robust_trust.compute_origin 
	    origin_a author_a new_chunks_10_a medit_10_l rev0_id rev0_uname in 
	(* Computes trust *)
        (* If the author is the same, we do not increase the reputation 
	   of exisiting text, to thwart a trivial attack. *)
        let (c_read_all, c_read_part) = 
          if rev0#get_user_id = rev1#get_user_id 
          then (0., 0.) 
          else (trust_coeff.read_all, trust_coeff.read_part)
        in 
        let (new_trust_10_a, new_sigs_10_a) = Compute_robust_trust.compute_robust_trust 
          trust_a sig_a new_chunks_10_a rev0_seps medit_10_l
          weight_user rev0_uid trust_coeff.lends_rep trust_coeff.kill_decrease 
          trust_coeff.cut_rep_radius c_read_all c_read_part trust_coeff.local_decay
        in 
	(* Now we have an estimate of trust, sigs, origin, author from
	   the latest revision, and the chunk lists already computed.
	   If the most similar revision to the current one is not the
	   immediately preceding revision, we try to improve the
	   estimate for the "live" chunk (chunk 0) by considering the
	   following chunks:
  	     closest revision -> live chunk
	     preceding revision -> dead chunk 1
	     dead chunks        -> dead chunks 2 ... n 
	   The origin will be due to this directly. 
	   For the trust, it computes the trust that would result from
           that edit, and assigns to each word the maximum trust that
           either this edit, or the edit 1 --> 0, would have
           computed. *)
        if !close_idx > 1 then begin 
          let rev2 = Vec.get !close_idx revs in 
	  (* Prepares the chunks of the previous revisions *)
	  let n_chunks_dual = (Array.length chunks_a) + 1 in 
	  let chunks_dual_a = Array.make n_chunks_dual [| |] in 
	  let trust_dual_a  = Array.make n_chunks_dual [| |] in 
	  let sig_dual_a   = Array.make n_chunks_dual  [| |] in 
	  let origin_dual_a = Array.make n_chunks_dual [| |] in 
	  let author_dual_a = Array.make n_chunks_dual [| |] in 
	  for i = 1 to n_chunks_dual - 2 do begin 
	    chunks_dual_a.(i + 1) <- chunks_a.(i);
	    trust_dual_a.(i + 1)  <- trust_a.(i);
	    sig_dual_a.(i + 1)   <- sig_a.(i);
	    origin_dual_a.(i + 1) <- origin_a.(i);
	    author_dual_a.(i + 1) <- author_a.(i);
	  end done;
	  (* rev1, the preceding one, is considered deleted, ... *)
	  chunks_dual_a.(1) <- rev1#get_words;
	  trust_dual_a.(1)  <- rev1#get_trust;
	  sig_dual_a.(1)   <- rev1#get_sigs;
	  origin_dual_a.(1) <- rev1#get_origin;
	  author_dual_a.(1) <- rev1#get_author;
	  (* ... while rev2, the most similar one, is considered to be the live one *)
	  chunks_dual_a.(0) <- rev2#get_words;
	  trust_dual_a.(0)  <- rev2#get_trust;
	  sig_dual_a.(0)   <- rev2#get_sigs;
	  origin_dual_a.(0) <- rev2#get_origin;
	  author_dual_a.(0) <- rev2#get_author;

          (* Analyzes this different chunk setup *)
          let (new_chunks_20_a, medit_20_l) = Chdiff.text_tracking chunks_dual_a rev0_t in 
	  (* Computes origin *)
	  let (new_origin_20_a, new_author_20_a) = Compute_robust_trust.compute_origin 
	    origin_dual_a author_dual_a new_chunks_20_a medit_20_l rev0_id rev0_uname in 
	  (* Keeps this origin information as the most reliable one. *)
	  new_origin_10_a.(0) <- new_origin_20_a.(0);
	  new_author_10_a.(0) <- new_author_20_a.(0);

          (* Computes the trust *)
          (* If the author is the same, we do not increase the reputation of exisiting text, 
             to thwart a trivial attack. *)
          let (c_read_all, c_read_part) = 
            if rev0#get_user_id = rev2#get_user_id 
            then (0., 0.) 
            else (trust_coeff.read_all, trust_coeff.read_part)
          in 
          let (new_trust_20_a, new_sigs_20_a) = Compute_robust_trust.compute_robust_trust
            trust_dual_a sig_dual_a new_chunks_20_a rev0_seps medit_20_l
            weight_user rev0_uid trust_coeff.lends_rep trust_coeff.kill_decrease 
            trust_coeff.cut_rep_radius c_read_all c_read_part trust_coeff.local_decay
          in
          (* The trust of each word is the max of the trust under both edits;
	     the signature is the signature of the max. *)
          for i = 0 to Array.length (new_trust_10_a.(0)) - 1 do
	    if new_trust_20_a.(0).(i) > new_trust_10_a.(0).(i) then begin 
	      new_trust_10_a.(0).(i) <- new_trust_20_a.(0).(i); 
	      new_sigs_10_a.(0).(i) <- new_sigs_20_a.(0).(i)
	    end
	  done;

        end; (* The closest version was not the immediately preceding one. *)
	(* After the case split of which version was the closest one, it is the
	   _10 variables that contain the correct values of trust and author 
	   signatures. *)

        (* Computes the list of deleted chunks with extended
           information (also age, timestamp), and the information for
           the live text *)
        del_chunks_list <- self#compute_dead_chunk_list new_chunks_10_a new_trust_10_a 
	  new_sigs_10_a new_origin_10_a new_author_10_a del_chunks_list medit_10_l rev1_time rev0_time;
        (* Writes the annotated markup, trust, origin, sigs to disk *)
        let buf = Revision.produce_annotated_markup rev0_seps new_trust_10_a.(0) 
	  new_origin_10_a.(0) new_author_10_a.(0) false true true in 
        db#write_colored_markup page_id rev0_id (Buffer.contents buf);
	rev0#set_trust  new_trust_10_a.(0);
	rev0#set_origin new_origin_10_a.(0);
	rev0#set_author new_author_10_a.(0);
	rev0#set_sigs   new_sigs_10_a.(0);
	rev0#write_words_trust_origin_sigs;
	(* Now that the colored revision is written out to disk, we don't need
	   any more its uncolored text.   If we are using the exec_api, 
	   we erase from the disk cache the text of all previous
	   revisions for the same page. *)
	db#erase_cached_rev_text page_id rev0_id rev0_time_string;
	(* Computes the overall trust of the revision. *)
	let t = Compute_robust_trust.compute_overall_trust new_trust_10_a.(0) in 
	rev0#set_overall_trust t  
      end (* method compute_trust *)


    (** [vote_for_trust voter_uid] increases the trust of a piece of text 
	due to voting by [voter_uid] *)
    method private vote_for_trust (voter_uid: int) : unit = 
      match work_revision_opt with
	None -> raise Missing_work_revision
      | Some rev0 -> begin
	  let rev0_id = rev0#get_id in
	  let rev0_t = rev0#get_words in 
	  let rev0_l = Array.length rev0_t in 
	  let rev0_seps = rev0#get_seps in 
	  (* Gets the voter reputation *)
	  let voter_rep = self#get_rep voter_uid in 
	  let voter_weight = self#weight (voter_rep) in 
	  (* Prepares the arguments for a call to compute_robust_trust *)
	  (* Prepares the arrays trust_a, sig_a as if they were for the previous
	     revision. *)
	  let trust_a =  [| rev0#get_trust |] in 
	  let sig_a   =  [| rev0#get_sigs  |] in 
	  (* New chunks array *)
	  let new_chunks_a = [| rev0_t     |] in 
	  (* Builds the edit list *)
	  let medit_l = [ Editlist.Mmov (0, 0, 0, 0, rev0_l) ] in 
	  (* Computes the new trust and signatures *)
	  let (new_trust_a, new_sigs_a) = Compute_robust_trust.compute_robust_trust 
            trust_a 
	    sig_a
            new_chunks_a 
            rev0_seps 
            medit_l
            voter_weight
	    voter_uid
            trust_coeff.lends_rep 
            trust_coeff.kill_decrease 
            trust_coeff.cut_rep_radius 
            trust_coeff.read_all
            0. (* trust_coeff.read_part *)
            trust_coeff.local_decay
	  in 
	  (* Writes the new colored markup *)
	  let buf = Revision.produce_annotated_markup rev0_seps new_trust_a.(0) 
	    rev0#get_origin rev0#get_author false true true in 
	  db#write_colored_markup page_id rev0_id (Buffer.contents buf);
	  (* Writes the trust information to the revision *)
	  rev0#set_trust new_trust_a.(0); 
	  rev0#set_sigs  new_sigs_a.(0);
	  rev0#write_words_trust_origin_sigs;
	  let t = Compute_robust_trust.compute_overall_trust new_trust_a.(0) in 
	  rev0#set_overall_trust t;
	  (* And writes the information on the revision back to disk. *)
	  if debug then !Online_log.online_logger#log "   Voted; writing the quality information...\n";
	  rev0#write_quality_to_db
	end (* end of vote_for_trust *)


    (** [compute_edit_inc]  computes the edit increments to reputation due to 
        the last revision.  These edit increments are computed on the 
        basis of all the revisions in the Vec revs: that is why the 
        pairwise distances in vec have been evaluated. 
        We use as input the reputation weight of the last user, who acts as the judge. *)
    method private compute_edit_inc : unit = 
      (* The Qual function (see paper) *)
      let qual d01 d12 d02 = begin 
	let qq = if d01 > 0. then (d02 -. d12) /. d01 else 1.
	in max (-1.) (min 1. qq)
      end in 
 
      (* This is the total number of revisions *)
      let n_revs = Vec.length revs in 
      (* This is the total number of recent revisions *)
      let n_recent_revs = Vec.length recent_revs in 

      (* The "triangle" of revisions is formed as follows: 
	 rev2 (newest, and judge); 
	 rev1 (judged);
	 r_c2 (the closest in distance to rev2 but prior to rev1).
       *)

      (* In the array of recent revisions, which one is the oldest to be judged? 
	 There are two cases: 
	 - If n_recent_revs < trust_coeff.n_revs_to_consider, this means that 
	   among the recent revisions is also the initial revision of the page.
	   In this case, we have to analize it, so the index of the oldest one
	   we analize is n_recent_revs - 1.
	   To analize this old revision, we compare with a virtual, empty initial
	   revision. 
	 - Otherwise, the oldest to be judged is n_recent_revs - 2, and we compare
	   it to the one of index n_recent_revs - 1 and perhaps others as well.
       *)
      let (include_initial_empty_rev, oldest_judged_idx) = 
	(* The test n_revs = n_recent_revs is there just for safety *)
	if n_recent_revs = trust_coeff.n_revs_to_consider && n_recent_revs = n_revs
	then (false, n_recent_revs - 2)
	else (true, n_recent_revs - 1)
      in
      if oldest_judged_idx > 0 then begin 

        let rev2 = Vec.get 0 revs in 
        let rev2_id    = rev2#get_id in 
        let rev2_uid   = rev2#get_user_id in 
        let rev2_uname = rev2#get_user_name in
        let rev2_time  = rev2#get_time in 
	let rev2_rep   = self#get_rep rev2_uid in 
	let rev2_weight = self#weight rev2_rep in 

	(* Reads the histogram of reputations, and the high median, and uses them
	   to renormalize the weight of the judging user.
	   NOTE: this implementation is thread-safe but not thread correct. 
	   Precisely, the histogram is updated correctly, but the median that is 
	   written out may not correspond to the correct median. 
	   This is done in order to avoid locking: as the median is used even for 
	   serving pages, we certainly do not want to write-lock the histogram 
	   information while updating it.  Any small inconsistency in the median 
	   value is temporary, and is largely irrelevant. *)
        (* In order to renormalize, we need to find out a lower bound for the work 
	   done by rev2; this is equal to the distance between rev2 and its closest
	   predecessor. *)
	let revp = Vec.get 1 revs in 
	let revp_id = revp#get_id in 
	let min_dist_to_2 = ref (Hashtbl.find edit_dist (revp_id, rev2_id)) in 
	for i = 2 to n_revs - 1 do begin 
	  let r = Vec.get i revs in 
	  let r_id = r#get_id in 
	  let dist_to_2 = Hashtbl.find edit_dist (r_id, rev2_id) in 
	  if dist_to_2 < !min_dist_to_2 then min_dist_to_2 := dist_to_2
	end done;
	(* The minimum distance from rev2 is now !min_dist_to_2 *)

	(* Renormalizes the reputation *)
	let (histogram, hi_median) = db#get_histogram in 
	let hi_median_boost = self#compute_hi_median histogram trust_coeff.hi_median_perc_boost in
	let renorm_w' = rev2_weight *. ((float_of_int max_rep_val) /. hi_median_boost) ** 1.0 in 
	let renorm_w = max rev2_weight (min renorm_w' (float_of_int max_rep_val)) in 

	(* If the author is not anonymous, updates the histogram *)
	if not_anonymous rev2_uid then begin 
	  (* Increments the histogram according to the work of the judge *)
	  let slot = max 0 (min 9 (int_of_float rev2_weight)) in 
	  histogram.(slot) <- histogram.(slot) +. !min_dist_to_2; 
	  !Online_log.online_logger#log (Printf.sprintf "\n Incrementing histogram slot %d by %f" 
	    slot !min_dist_to_2);
	  let new_hi_median' = self#compute_hi_median histogram trust_coeff.hi_median_perc in 
	  new_hi_median <- max hi_median new_hi_median';
	  (* Produces the array of differences *)
	  delta_hist.(slot) <- !min_dist_to_2;
	  histogram_updated <- true;
	end;

	(* We compute the reputation scaling dynamically taking care
	   of the size of the recent_revision list and the union of
	   the recent revision list, high reputation list and high
	   trust list. *)
	let dynamic_rep_scaling_factor = trust_coeff.dynamic_rep_scaling n_recent_revs 
	  trust_coeff.n_revs_to_consider in

	for rev1_idx = 1 to oldest_judged_idx do begin 
	  (* Remembers if rev1 is the first one of the page *)
	  let rev1_is_first_page_revision = (rev1_idx = oldest_judged_idx) && include_initial_empty_rev in 
          let rev1 = Vec.get rev1_idx revs in 
          let rev1_uid   = rev1#get_user_id in 
	  (* We work only on non-anonymous rev1; otherwise, there is nothing to be updated. 
	     Moreover, rev1 and rev2 need to be by different authors. *)
	  if (not_anonymous rev1_uid) && (rev1_uid <> rev2_uid) then begin 
            let rev1_id    = rev1#get_id in 
            let rev1_uname = rev1#get_user_name in 
            let rev1_time  = rev1#get_time in 
	    let rev1_nix   = ref (rev1#get_nix) in 
	    let rev1_rep   = self#get_rep rev1_uid in 
	    let d12        = Hashtbl.find edit_dist (rev2_id, rev1_id) in 

	    (* The revision r_c2 is the revision prior to rev1, and closest to rev2.
	       We compute some quantities for it. *)
	    let (r_c2_id, r_c2_uid, r_c2_username, r_c2_rep, d_c2_1, d_c2_2, delta) = 
	      if rev1_is_first_page_revision then begin 
		(* If we are analyzing the first revision, r_c2 is the empty revision that 
		   implicitly precedes rev1. *)
		let len1 = Array.length (rev1#get_words) in 
		(0,  (* The id is 0, so we track it in the log output *)
		0,   (* The uid is 0 (what else?) *)
		"Virtual_empty_initial_revision",  (* username *)
		(* The empty revision is empty with very high
		   reputation!  This means that, for the reputation
		   cap formulas, the only reputations that matter are
		   those for the revisions that follow it. *)
		trust_coeff.max_rep,
		(* The distance between the empty revision, and rev1,
		   is the length of rev1... *)
		float_of_int len1, 
		(* and similarly for the distance to rev2. *)
		float_of_int (Array.length (rev2#get_words)),
		(* delta is obviously len1 *)
		float_of_int len1
		)
	      end else begin 
		(* If we are not analyzing the first revision, searches for r_c2 *)
		let revp = Vec.get (rev1_idx + 1) revs in 
		let revp_id = revp#get_id in 
		let min_dist_to_1 = ref (Hashtbl.find edit_dist (revp_id, rev1_id)) in 
		let min_dist_to_2 = ref (Hashtbl.find edit_dist (revp_id, rev2_id)) in 
		let r_c1  = ref revp in 
		let r_c2  = ref revp in 
		for i = rev1_idx + 2 to n_revs - 1 do begin 
		  let r = Vec.get i revs in 
		  let r_id = r#get_id in 
		  let dist_to_1 = Hashtbl.find edit_dist (r_id, rev1_id) in 
		  let dist_to_2 = Hashtbl.find edit_dist (r_id, rev2_id) in 
		  if dist_to_1 < !min_dist_to_1 then begin 
		    min_dist_to_1 := dist_to_1;
		    r_c1  := r
		  end;
		  if dist_to_2 < !min_dist_to_2 then begin 
		    min_dist_to_2 := dist_to_2;
		    r_c2  := r
		  end
		end done;
		let r_c2_id = (!r_c2)#get_id in
		let r_c2_uid = (!r_c2)#get_user_id in 
		(* outputs the results *)
		(r_c2_id,
		r_c2_uid,
		(!r_c2)#get_user_name,
		self#get_rep r_c2_uid, 
		Hashtbl.find edit_dist (r_c2_id, rev1_id), 
		!min_dist_to_2,
		!min_dist_to_1
		)
	      end 
	    in 
	    (* Computes the quality due to r_c2, rev1, rev2 *)
	    let q = qual d_c2_1 d12 d_c2_2 in 

	    (* computes the nixing bit *)
	    let oldest_of_recent_revs = Vec.get (n_recent_revs - 1) revs in 
	    let oldest_of_recent_revs_time = oldest_of_recent_revs#get_time in 
	    if (not !rev1_nix) && (rev2_time -. rev1_time < trust_coeff.nix_interval) then begin 
	      (* You can be nixed in two ways. *)
	      if 
		(* First reason: if the quality is below the threshold *)
		(q <= trust_coeff.nix_threshold) ||
		  (* Second reason: too many revisions in too short a time *)
		  ((not include_initial_empty_rev) && 
		  (rev1_time -. oldest_of_recent_revs_time < trust_coeff.nix_interval))
	      then begin 
		(* Nix it *)
		rev1_nix := true;
		rev1#set_nix_bit;
		let s = 
		  if (q <= trust_coeff.nix_threshold)
		  then Printf.sprintf "\n\nRevision %d has been nixed due to quality" rev1_id
		  else Printf.sprintf "\n\nRevision %d has been nixed due to too frequent edits" rev1_id
		in !Online_log.online_logger#log s
	      end
	    end;

            (* We compute the reputation increment by the local
               feedback for rev1, where rev1_prev is the immediate
               previous revision of rev1. In this case, we always apply
               the repuation cap, where the cap is the minimum of the
               repuation of the judging revision rev2 and the rev1_prev
               (this is because the previous version rev1_prev can be
               under control of the author of rev1).  The reputation
               after local feedback is obtained as rev1_local, and we
               take the maximum of the local feedback and the
               reputation computation by improve-the-past
               algorithm. Since both are robust, the robust of the
               algorithm is ensured *)
            (* Computes reputation after local feedback*) 
	    let rev1_local = 
	      if rev1_is_first_page_revision then begin
		(* This is basically a no-op *)
		rev1_rep 
	      end else begin  
		let rev1_prev        = Vec.get (rev1_idx + 1) revs in 
		let rev1_prev_id     = rev1_prev#get_id in 
		let rev1_prev_uid    = rev1_prev#get_user_id in 
		let rev1_prev_rep    = self#get_rep rev1_prev_uid in 
		let dist_prev1       = Hashtbl.find edit_dist (rev1_prev_id, rev1_id) in 
		let dist_prev2       = Hashtbl.find edit_dist (rev1_prev_id, rev2_id) in 
		let q_local          = qual dist_prev1 d12 dist_prev2 in 
		let delta_local      = dist_prev1 in
		let rep_inc_local    = dynamic_rep_scaling_factor *. delta_local *. q_local *. renorm_w in
		let cap_rep_local    = min rev2_rep rev1_prev_rep in
		let capped_rep_local = min cap_rep_local (rev1_rep +. rep_inc_local) in 
		max rev1_rep capped_rep_local
	      end
	    in
	    (* Computes the uncapped reputation increment *)
	    let rep_inc = dynamic_rep_scaling_factor *. delta *. q *. renorm_w in
	       
	    (* Applies the reputation increment according to reputation cap *)
	    let new_rep = 
	      if rep_inc < 0. then begin
		(* Negative increment.  Reputation cannot become negative *)
		max 0. (rev1_rep +. rep_inc)
	      end else begin
		(* Positive increment. *)
		if (!rev1_nix || rev2_time -. rev1_time < trust_coeff.nix_interval) 
		  && rep_inc > 0. then begin 
		    (* Short term or nixed: caps the reputation increment. 
		       The reputation of rev2 is always used as a cap. 
		       The reputation of r_c2 is used as a cap only if it is 
		       more recent than the nixing interval. *)
		    let r_c2_cap_rep = 
		      if rev2_time -. oldest_of_recent_revs_time < trust_coeff.nix_interval
		      then r_c2_rep
		      else rev2_rep
		    in
		    let cap_rep = min rev2_rep r_c2_cap_rep in
		    let capped_rep = min cap_rep (rev1_rep +. rep_inc) in 
		    max (max rev1_rep rev1_local) capped_rep    
		  end else begin 
		    (* uncapped reputation increment *)
		    max rev1_local (rev1_rep +. rep_inc)      
		  end
	      end
	    in 
	    self#set_rep rev1_uid new_rep;

	    (* Adds quality information for the revision *)
	    rev1#add_edit_quality_info delta q (new_rep -. rev1_rep) ; 

            (* For logging purposes, produces the Edit_inc line *)
            !Online_log.online_logger#log (Printf.sprintf "\n\nEditInc %10.0f PageId: %d Inc: %.4f Capd_inc: %.4f q: %.4f Delta: %.2f" 
	      (* time and page id *)
	      rev2_time page_id rep_inc (new_rep -. rev1_rep) q delta);
	    (* revision and user ids *)
	    !Online_log.online_logger#log (Printf.sprintf "\n  rev_c2: %d uid_c2: %d uname_c2: %S rev_c2_rep: %.3f" 
	      r_c2_id r_c2_uid r_c2_username r_c2_rep); 
	    !Online_log.online_logger#log (Printf.sprintf "\n  rev1: %d uid1: %d uname1: %S r1_rep: %.3f Nixed: %B" 
	      rev1_id rev1_uid rev1_uname rev1_rep rev1#get_nix); 
	    !Online_log.online_logger#log (Printf.sprintf "\n  rev2: %d uid2: %d uname2: %S r2_rep: %.3f w2_renorm: %.3f" 
	      rev2_id rev2_uid rev2_uname rev2_rep renorm_w); 
	    !Online_log.online_logger#log (Printf.sprintf "\n  d_c1_1: %.2f d_c2_1: %.2f d_c2_2: %.2f d12: %.2f rev_1_to_2_time: %.3f\n"
	      delta d_c2_1 d_c2_2 d12 ((rev2_time -. rev1_time) /. (3600. *. 24.))); 
	  end (* rev1 is by non_anonymous *)
	end done (* for rev1_idx *)
      end (* if oldest_judged_idx > 0 , and method compute_edit_inc *)


    (** This method computes the trust of the revision 0, given that
        the edit distances from previous revisions are known.  The
        method is as follows: it compares the newest revision 0 with
        both the preceding one, 1, and with the closest to 0 in edit
        distance, if different from 1.  The trust is then computed as
        the maximum of the two figures.  The method computes also the
        effects on author reputation as a result of the new revision.
        It returns a flag indicating whether anything has been
        done. *)
    method eval : bool = 

      (* Keep track of whether the revision needs coloring, on how
	 many tries we have made *)
      let needs_coloring = ref false in 
      let done_something = ref true in 
      let n_attempts = ref 0 in 

      (* This is the main transaction body.  We do in this body the
	 computation that needs to be consistent for a page. *)
      while !n_attempts < n_retries do 
	begin 
	  try begin 
	    
	    db#start_transaction;
	    
	    (* We do something only if the revision needs coloring *)
	    needs_coloring := db#revision_needs_coloring page_id revision_id;
	    if !needs_coloring then begin
	      
	      (* Reads the previous revisions *)
	      self#read_page_revisions_edit; 
	      
	      (* Computes the edit distances *)
	      if debug then !Online_log.online_logger#log "   Computing edit lists...\n";
	      self#compute_edit_lists; 
	      (* Computes, and writes to disk, the trust of the newest revision *)
	      if debug then !Online_log.online_logger#log "   Computing trust...\n";
	      self#compute_trust;
	      
	      (* We now process the reputation update. *)
	      if debug then !Online_log.online_logger#log "   Computing edit incs...\n";
	      self#compute_edit_inc;
	      
	      (* Inserts the revision in the list of high rep or high trust revisions, 
		 and deletes old signatures *)
	      self#insert_revision_in_lists;
	      (* We write to disk the page information *)
	      db#write_page_chunks_info page_id del_chunks_list page_info;

	      (* We write back to disk the information of all revisions *)
	      if debug then !Online_log.online_logger#log "   Writing the quality information...\n";
	      let f r = r#write_quality_to_db in 
	      Vec.iter f revs;

	      db#commit;
	      n_attempts := n_retries

	    end else begin
	      (* The revision is already colored; there is nothing to do *)
	      db#commit;
	      done_something := false;
	      n_attempts := n_retries
	    end

	  end (* try: this is the end of the main transaction *)
	  with Online_db.DB_TXN_Bad -> begin 
	    (* Roll back *)
	    db#rollback_transaction;
	    n_attempts := !n_attempts + 1
	  end
	end done; (* End of the multiple attempts at the transaction *)

      (* If the revision needed coloring, we need to write reputations and revision 
	 quality information to disk *)
      if !needs_coloring then begin 

	(* We write to disk all reputation changes *)
	if debug then !Online_log.online_logger#log "   Writing the reputations...\n";
	self#write_all_reps;
	
	if debug then !Online_log.online_logger#log "   All done!\n";
      end; (* needs coloring *)

      (* Writes the new histogram *)
      if histogram_updated then begin 
	let n_attempts = ref 0 in 
	while !n_attempts < n_retries do 
	  begin 
	    try 
	      begin 
		db#start_transaction;
		db#write_histogram delta_hist new_hi_median;
		db#commit;
		histogram_updated <- false;
		n_attempts := n_retries
	      end 
	    with Online_db.DB_TXN_Bad -> 
	      begin 
		(* Roll back *)
		db#rollback_transaction;
		n_attempts := !n_attempts + 1
	      end
	  end done
      end;

      (* Flushes the logger.  *)
      !Online_log.online_logger#flush; 

      (* Returns whether we have done something *)
      !done_something
    (* End of eval method *)

    (** [vote voter_uid] is called to process the fact that user [voter_uid] 
	has pushed on the "I agree with this revision" button. 
        The method only does somethign if the revision is the last 
        for the page, and returns, in a flag, whether it has done 
        something or not. *)
    method vote (voter_id: int) : bool = 

      (* Keeps track of whether we have done any work *)
      let done_something = ref false in 

      (* Start of transaction, retrying many times if needed *)
      let n_attempts = ref 0 in 
      while !n_attempts < n_retries do 
	begin 
	  try 
	    db#start_transaction;
	    if debug then !Online_log.online_logger#log "Start vote for revision...\n";
	    (* Reads the page information and the revision *)
	    self#read_page_revisions_vote; 
	    (* Increases the trust of the revision, and writes the 
	       results back to disk *)
	    self#vote_for_trust voter_id; 
	    (* Inserts the revision in the list of high rep or high
	       trust revisions, and deletes old signatures *)
	    self#insert_revision_in_lists;
	    (* We write to disk the page information *)
	    db#write_page_info page_id page_info;
	    (* Commits *)
	    db#commit;
	    n_attempts := n_retries;
	    done_something := true;
	    !Online_log.online_logger#log (Printf.sprintf 
	      "\n\nUser %d voted for revision %d of page %d" 
	      voter_id revision_id page_id);
	    !Online_log.online_logger#flush; 
	    
	  with Online_db.DB_TXN_Bad | Online_db.DB_Not_Found -> begin 
	    db#rollback_transaction;
	    n_attempts := !n_attempts + 1
	  end 
	end done; (* End of the multiple attempts at the transaction *)
	(* Returns whether we have done something *)
	!done_something
	(* End of vote method *)

  end (* class *)

