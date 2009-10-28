(*

Copyright (c) 2009 The Regents of the University of California
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


(** This class adds to a wiki dump the information on text trust,
    origin, and author, producing the "colored revisions".  The class
    also outputs the sql statements that can be used to prime the
    online system with the proper information.  *)

(* TODO(Luca): we could use a more sophisticated comparison, as in the
   online system, where a revision would be compared with multiple
   past ones. *)

type word = string 
exception Ins_text_in_deleted_chunk
open Eval_defs
open Online_types
open Mysql
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib

(* This is the function that sexplib uses to convert floats *)
Sexplib.Conv.default_string_of_float := (fun n -> Printf.sprintf "%.3f" n);;

class page 
  (page_id: int)
  (page_title: string)
  (* File to use for sql command output *)
  (sql_file: out_channel)
  (* Base path for filesystem revision storage, if requested. *)
  (colored_base_path: string)
  (* Prefix for db tables *)
  (db_prefix: string)
  (* History of user reputations *)
  (rep_histories: Rephist.rephist)
  (* Coefficients for trust computation *)
  (trust_coeff_lends_rep: float)
  (trust_coeff_read_all: float) 
  (trust_coeff_read_part: float) 
  (trust_coeff_local_decay: float)
  (trust_coeff_cut_rep_radius: float) 
  (trust_coeff_kill_decrease: float)
  (* N. of signatures to output *)
  (n_sigs: int) 
  (* Robots *)
  (robots: Read_robots.robot_set_t)
  (edit_time_constant: float)
  =

object(self) 

    (* This is a dynamically modifiable vector of revisions, used as a
       buffer.  revs[0] is the oldest, and is the revision
       number offset (see later, offset is a field of page) for
       the page. *)
    val mutable revs : Revision.trust_revision Vec.t = Vec.empty 
      (* In the Vec implementation, offset is the offset of the oldest
         (position 0 in revs) revision. *)
    val mutable offset : int = 0

      (* Arrays of chunks and chunk attributes for the last version of
	 the page. *)
    (* chunks_a is a word array array, and is used to represent both
       the live text (element 0) or the dead text (elements >0) of a
       page. *)
    val mutable chunks_a : word array array = [| [| |] |] 
      (* This float array array stores a float for each word, and is
         used to store the trust of each word. *)
    val mutable chunks_trust_a  : float array array = [| [| |] |] 
      (* This array keeps track of the revision id in which each word 
	 was introduced. *)
    val mutable chunks_origin_a : int array array = [| [| |] |]
      (* This array keeps track of the author of each word *)
    val mutable chunks_author_a : string array array = [| [| |] |]
      (* This array keeps track of the author sigs *)
    val mutable chunks_sig_a : Author_sig.packed_author_signature_t array array = [| [| |] |]
      (* Writer for blobs *)
    val blob_writer = new Revision_writer.writer 
      page_id None (Some colored_base_path) None false
      (* Last blob_id *)
    val mutable blob_id : int = Online_types.blob_locations.initial_location


    method print_id_title : unit = ()


    (** Writes the SQL code for writing the wikitrust_revision to the db. *)
    method private write_wikitrust_revision_sql 
      (r: Revision.trust_revision) : unit = 
      (* Revision parameters *)
      let rev_id = ml2int r#get_id in
      let page_id = ml2int r#get_page_id in
      (* We don't have the text_id, so we put 0 in place. *)
      let text_id = ml2int 0 in
      let time_string = ml2str (Timeconv.compact_time_string r#get_timestamp) in
      let user_id = ml2int r#get_user_id in
      let username = ml2str r#get_user_name in
      let is_minor = ml2int (if r#get_is_minor then 1 else 0) in 
      (* Quality parameters *)
      (* I do field-by-field initialization, rather than copying the whole
	 structure, because otherwise we get two references to the SAME
	 structure, unfortunately. *)
      let trust_a = r#get_word_trust in
      let quality_info : qual_info_t = {
	n_edit_judges = quality_info_default.n_edit_judges;
	total_edit_quality = quality_info_default.total_edit_quality;
	min_edit_quality = quality_info_default.min_edit_quality;
	nix_bit = quality_info_default.nix_bit;
	delta = quality_info_default.delta;
	reputation_gain = quality_info_default.reputation_gain;
	overall_trust = Compute_robust_trust.compute_overall_trust trust_a;
	word_trust_histogram = 
	  Compute_robust_trust.compute_trust_histogram trust_a;
      } in
      (* Prepares these parameters. *)
      let db_qual_info = ml2str 
	(string_of__of__sexp_of sexp_of_qual_info_t quality_info) in 
      let q2 =  ml2float quality_info.reputation_gain in 
      let aq2 = if (q2 = "inf") then (ml2float infinity) else q2 in
      let db_overall_trust = ml2float quality_info.overall_trust in
      let db_overall_quality = ml2float 0.0 in
      (* Db write access *)
      Printf.fprintf sql_file "INSERT INTO %swikitrust_revision (revision_id, page_id, text_id, time_string, user_id, username, is_minor, quality_info, blob_id, reputation_delta, overall_trust, overall_quality) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s);\n"
	db_prefix rev_id page_id text_id time_string user_id username is_minor 
	db_qual_info (ml2int blob_id) aq2 db_overall_trust db_overall_quality
	

    (** Computes the distances between the newest revision and all previous ones. *)
    method private compute_distances : unit =
      (* gets last version *)
      let rev2_idx = (Vec.length revs) - 1 in
      let rev2 = Vec.get rev2_idx revs in 
      (* gets text etc of last version *) 
      let rev2_t = rev2#get_words in 
      let rev2_l = Array.length (rev2_t) in 

      (* Loop over some preceding revisions *)
      for rev1_idx = rev2_idx - 1 downto 0 do begin
        let i = rev2_idx - rev1_idx in 
        let rev1 = Vec.get rev1_idx revs in 
        let rev1_t = rev1#get_words in 
        let rev1_l = Array.length (rev1_t) in 

        if rev1_idx + 1 = rev2_idx 
	  (* Computes the precise distance and edit list between
	     the two last revisions. *)
	then begin 
          let edits  = Chdiff.edit_diff rev1_t rev2_t in 
          let d      = Editlist.edit_distance edits (max rev1_l rev2_l) in 
          rev1#set_distance (Vec.setappend 0.0 d i rev1#get_distance);
          rev1#set_editlist (Vec.setappend [] edits i rev1#get_editlist);
	end
	else begin 
	  (* Computes the distance between rev2 and rev1.  The 
	     computation uses edit list zipping, if possible. *)
	  (* First, we need to pick the best interpolant between 
	     rev1 and rev2. *)
	  let best_middle_idx = ref (-1) in 
          let best_coverage   = ref (rev1_l + rev2_l + 1) in 
          for revm_idx = rev2_idx - 1 downto rev1_idx + 1 do begin 
            let revm = Vec.get revm_idx revs in 
            let revm_e = Vec.get (rev2_idx - revm_idx) revm#get_editlist in 
            let forw_e = Vec.get (revm_idx - rev1_idx) rev1#get_editlist in 
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
            if perc <= max_perc_to_zip && unc <= max_uncovered_to_zip 
	      && unc < !best_coverage then begin 
		best_coverage := unc; 
		best_middle_idx := revm_idx
              end
          end done; 

          (* If it found anything suitable, uses it *)
          if !best_middle_idx > -1 then begin 
            (* Then uses the best middle index to zip *)
            let revm = Vec.get !best_middle_idx revs in 
            let revm_e = Vec.get (rev2_idx - !best_middle_idx) revm#get_editlist in 
            let forw_e = Vec.get (!best_middle_idx - rev1_idx) rev1#get_editlist in 
            (* ... and computes the distance via zipping. *)
            let edits = Compute_edlist.edit_diff_using_zipped_edits 
	      rev1_t rev2_t forw_e revm_e in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
            rev1#set_distance (Vec.setappend 0.0 d i rev1#get_distance);
            rev1#set_editlist (Vec.setappend [] edits i rev1#get_editlist);

          end else begin 
            (* Nothing suitable found, uses the brute-force approach of computing 
	       the edit distance from direct text comparison. ¯*)
            let edits   = Chdiff.edit_diff rev1_t rev2_t in 
            let d = Editlist.edit_distance edits (max rev1_l rev2_l) in 
            rev1#set_distance (Vec.setappend 0.0 d i rev1#get_distance);
            rev1#set_editlist (Vec.setappend [] edits i rev1#get_editlist);
          end

	end (* if the distance is not being computed wrt the previous revision *)
      end done (* loop over preceding revisions *)


    (** Processes a new revision, computing trust, author, and origin, 
	and outputting:
	- The colored revision text, compressed, in the filesystem, if 
	  requested.
	- The sql code for the online system metadata.
	- The colored revisions in an xml file. *)
    method private eval_newest : unit = 
      let rev_idx = (Vec.length revs) - 1 in 
      let rev = Vec.get rev_idx revs in 
      let uid = rev#get_user_id in 
      let rev_time = rev#get_time in 
      (* Gets the reputation of the author of the current revision *)
      let weight = rep_histories#get_precise_weight uid in 
      let new_wl = rev#get_words in 
      (* Fixes the coefficients of trust incease depending on whether
	 the user is a bot... *)
      let (read_all', read_part') = 
	if is_user_a_bot robots rev#get_user_name
	then (0., 0.)
	else (trust_coeff_read_all, trust_coeff_read_part)
      in
      (* ...and depending on the time interval wrt. the previous edit *)
      let (read_all, read_part) =
	if rev_idx = 0
	then (read_all', read_part')
	else begin
	  let prev_rev = Vec.get (rev_idx - 1) revs in
	  let prev_time = prev_rev#get_time in
	  let delta_time = max 0. (rev_time -. prev_time) in
	  let time_factor = 1. -. exp (0. -. delta_time /. edit_time_constant)
	  in (
	    1. -. (1. -. read_all')  ** time_factor,
	    1. -. (1. -. read_part') ** time_factor)
	end
      in

      (* Calls the function that analyzes the difference 
         between revisions. Data relative to the previous revision
         is stored in the instance fields chunks_a and chunks_attr_a *)
      let (new_chunks_10_a, medit_10_l) = Chdiff.text_tracking chunks_a new_wl in 
      (* Computes the origin of the words in the new revision. *)
      let (new_origin_10_a, new_author_10_a) = 
	Compute_robust_trust.compute_origin 
	  chunks_origin_a chunks_author_a new_chunks_10_a medit_10_l 
	  rev#get_id rev#get_user_name 
      in 
      (* Computes the trust, and the sigs *)
      let (new_trust_10_a, new_sigs_10_a) = 
	Compute_robust_trust.compute_robust_trust
	  chunks_trust_a chunks_sig_a new_chunks_10_a 
	  rev#get_seps medit_10_l weight uid 
	  trust_coeff_lends_rep trust_coeff_kill_decrease 
	  trust_coeff_cut_rep_radius read_all read_part 
	  trust_coeff_local_decay 
      in
      (* We check if there is some recent revision closer than the 
	 immediately preceding one. *)
      let closest_idx = ref (rev_idx - 1) in
      let closest_d = ref 0. in
      for past_rev_idx = rev_idx - 1 downto 0 do begin
	(* Gets the distance d between the current revision
	   and the one at past_rev_idx *)
	let past_rev = Vec.get past_rev_idx revs in
	let d = Vec.get (rev_idx - past_rev_idx) past_rev#get_distance in
	if past_rev_idx = rev_idx - 1 || d < !closest_d then begin
	  closest_idx := past_rev_idx;
	  closest_d := d
	end
      end done;

      (* If the closest revision is not the last one, we check whether we
	 get a better comparison using that one instead. *)
      if !closest_idx <> rev_idx - 1 then begin
	(* rev1 is the previous revision, and rev2 is the closest *)
	let rev1 = Vec.get (rev_idx - 1) revs in
	let rev2 = Vec.get !closest_idx revs in
	(* Prepares the chunks for comparing with this older revision. *)
	let n_chunks_dual = (Array.length chunks_a) + 1 in 
	let chunks_dual_a = Array.make n_chunks_dual [| |] in 
	let trust_dual_a  = Array.make n_chunks_dual [| |] in 
	let sig_dual_a   = Array.make n_chunks_dual  [| |] in 
	let origin_dual_a = Array.make n_chunks_dual [| |] in 
	let author_dual_a = Array.make n_chunks_dual [| |] in 
	for i = 1 to n_chunks_dual - 2 do begin 
	  chunks_dual_a.(i + 1) <- chunks_a.(i);
	  trust_dual_a.(i + 1)  <- chunks_trust_a.(i);
	  sig_dual_a.(i + 1)    <- chunks_sig_a.(i);
	  origin_dual_a.(i + 1) <- chunks_origin_a.(i);
	  author_dual_a.(i + 1) <- chunks_author_a.(i);
	end done;
	(* rev1, the preceding one, is considered deleted, ... *)
	chunks_dual_a.(1) <- rev1#get_words;
	trust_dual_a.(1)  <- rev1#get_word_trust;
	sig_dual_a.(1)    <- rev1#get_word_sig;
	origin_dual_a.(1) <- rev1#get_word_origin;
	author_dual_a.(1) <- rev1#get_word_author;
	(* ... while rev2, the most similar one, is considered to be
	   the live one *)
	chunks_dual_a.(0) <- rev2#get_words;
	trust_dual_a.(0)  <- rev2#get_word_trust;
	sig_dual_a.(0)    <- rev2#get_word_sig;
	origin_dual_a.(0) <- rev2#get_word_origin;
	author_dual_a.(0) <- rev2#get_word_author;
	
        (* Analyzes this different chunk setup *)
        let (new_chunks_20_a, medit_20_l) = Chdiff.text_tracking chunks_dual_a new_wl in 
	(* Computes origin *)
	let (new_origin_20_a, new_author_20_a) = Compute_robust_trust.compute_origin 
	  origin_dual_a author_dual_a new_chunks_20_a medit_20_l 
	  rev#get_id rev#get_user_name in 
	(* Keeps this origin information as the most reliable one. *)
	new_origin_10_a.(0) <- new_origin_20_a.(0);
	new_author_10_a.(0) <- new_author_20_a.(0);
	
        (* Computes the trust *)
        let (new_trust_20_a, new_sigs_20_a) = 
	  Compute_robust_trust.compute_robust_trust
            trust_dual_a sig_dual_a new_chunks_20_a rev#get_seps medit_20_l
            weight uid trust_coeff_lends_rep 
	    trust_coeff_kill_decrease 
            trust_coeff_cut_rep_radius read_all read_part 
	    trust_coeff_local_decay
        in
        (* The trust of each word is the max of the trust under both edits;
	   the signature is the signature of the max. *)
        for i = 0 to Array.length (new_trust_10_a.(0)) - 1 do
	  if new_trust_20_a.(0).(i) > new_trust_10_a.(0).(i) then begin 
	    new_trust_10_a.(0).(i) <- new_trust_20_a.(0).(i); 
	    new_sigs_10_a.(0).(i)  <- new_sigs_20_a.(0).(i)
	  end
	done

      end; (* The closest revision is not the preceding one. *)
	(* After the case split of which version was the closest one, it is the
	   _10 variables that contain the correct values of trust and author 
	   signatures. *)

      (* Replaces the chunks for the next iteration *)
      chunks_trust_a  <- new_trust_10_a;
      chunks_origin_a <- new_origin_10_a; 
      chunks_author_a <- new_author_10_a;
      chunks_sig_a    <- new_sigs_10_a;
      chunks_a        <- new_chunks_10_a;
      (* Also notes in the revision the reputations and the origins,
	 as well as the author_sigs. *)
      rev#set_word_trust  chunks_trust_a.(0);
      rev#set_word_origin chunks_origin_a.(0);
      rev#set_word_author chunks_author_a.(0);
      rev#set_word_sig    chunks_sig_a.(0);

      (* Outputs the colored text to the blob. *)
      blob_id <- blob_writer#write_revision rev#get_id rev#get_colored_text;
      (* Now we have to write the metadata for sql. *)
      self#write_wikitrust_revision_sql rev


    (** This method is called to add a new revision to be evaluated
        for trust.  Note that here, we do analyze revisions, even
        thought they might be from the same author.  Signatures
        prevent trust from raising when it should not. *)
    method add_revision 
      (rev_id: int) (* revision id *)
      (page_id: int) (* page id *)
      (timestamp: string) (* timestamp string *)
      (time: float) (* time, as a floating point *)
      (contributor: string) (* name of the contributor *)
      (user_id: int) (* user id *)
      (ip_addr: string)
      (username: string) (* name of the user *)
      (is_minor: bool) 
      (comment: string)
      (text_init: string Vec.t) (* Text of the revision, still to be 
				   split into words *)
      : unit =
      (* First, we have to "disarm" the text from the xml tag
	 conversions, so that &gt; is transformed into >, and so
	 forth. *)
      let disarmed_text = Vec.map Text.xml_disarm text_init in
      let r = new Revision.trust_revision rev_id page_id timestamp time 
	contributor user_id ip_addr username is_minor comment 
	disarmed_text true false in 
      (* Adds the revision to the Vec of revisions. *)
      revs <- Vec.append r revs; 
      (* Computes all the distances from this new revision to the previous ones. *)
      self#compute_distances;
      (* Evaluates the newest version *)
      self#eval_newest; 
      (* If the buffer is full, evaluates the oldest version and kicks
	 it out *)
      if (Vec.length revs) > n_sigs then begin 
	(* The parameter 0 is the index of what is considered to be
           the oldest.  It is used, since in no_more_revisions it may
           be a larger number *)
	revs <- Vec.remove 0 revs;
	(* increments the offset of the oldest version *)
	offset <- offset + 1 
      end (* if *)
	  

    (** This method produces the sql code that adds the wikitrust_page
	information to the db. *)
    method private produce_page_information (open_blob_id: int) : unit =
      let page_info = {
	past_hi_rep_revs = [];
	past_hi_trust_revs = [];
      } in 
      let info_string_db = ml2str 
	(string_of__of__sexp_of sexp_of_page_info_t page_info) in 
      Printf.fprintf sql_file "INSERT INTO %swikitrust_page (page_id, page_title, page_info, last_blob) VALUES (%s, %s, %s, %s);\n"
	db_prefix (ml2int page_id) (ml2str page_title) 
	info_string_db (ml2int open_blob_id)


    (** This method writes the chunks to their own blob. *)
    method private write_chunks : unit = 
      (* We need to produce a chunk_t list first. *)
      (* I timestamp them all with the time of the current revision.
	 This is not ideal, but will be fine. *)
      let rev_idx = (Vec.length revs) - 1 in 
      let rev = Vec.get rev_idx revs in 
      let rev_time = rev#get_time in 
      let chunk_list = ref [] in 
      for i = 1 to (Array.length chunks_a) - 1 do begin 
	let c = {
	  timestamp = rev_time;
	  n_del_revisions = 0;
	  text = chunks_a.(i);
	  trust = chunks_trust_a.(i);
	  sigs = chunks_sig_a.(i);
	  origin = chunks_origin_a.(i);
	  author = chunks_author_a.(i);
	} in 
	chunk_list := c :: !chunk_list
      end done;
      let chunks_string = string_of__of__sexp_of 
	(sexp_of_list sexp_of_chunk_t) !chunk_list in 
      (* Writes the chunks *)
      Revision_store.write_blob colored_base_path page_id 
	Online_types.blob_locations.chunks_location chunks_string


    (** This method writes the sigs of the last few revisions in the blob. *)
    method private write_sigs : unit = 
      (* f is folded on the Vec of revisions, producing a list of
	 (id, sig) that is ready to be written to disk *)
      let f r l = (r#get_id, r#get_sig) :: l in
      let sig_list : page_sig_disk_t = Vec.fold f revs [] in
      let sig_string = 
	string_of__of__sexp_of sexp_of_page_sig_disk_t sig_list in
      Revision_store.write_blob colored_base_path page_id
	Online_types.blob_locations.sig_location sig_string


    (** This method is called when there are no more revisions to evaluate. 
	We need to produce the sql that contains the page information. *)
    method eval: unit = 
      (* Finishes writing all the blobs *)
      let last_blob_id = blob_writer#close in
      (* Produces the page information. *)
      self#produce_page_information last_blob_id;
      (* Outputs chunks and sigs *)
      self#write_chunks;
      self#write_sigs
    
  end
