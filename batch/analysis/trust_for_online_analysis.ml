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


(** This class adds to a wiki dump the information on text trust, origin, and author,
    producing the "colored revisions". 
    The class also outputs the sql statements that can be used to prime the online
    system with the proper information.
 *)

(* TODO(Luca): we could use a more sophisticated comparison, as in the online system,
   where a revision would be compared with multiple past ones. *)

type word = string 
exception Ins_text_in_deleted_chunk
open Eval_defs
open Online_types
open Mysql
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib

class page 
  (page_id: int)
  (title: string)
  (* File to use for xml colored revision output *)
  (xml_file: out_channel)
  (* File to use for sql command output *)
  (sql_file: out_channel)
  (* Base path for filesystem revision storage, if requested. *)
  (colored_base_path: string option)
  (* Base path for filesystem signature storage, if requested. *)
  (sig_base_path: string option)
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


    (* We print the page information in the xml file *)
    method print_id_title : unit =
      match colored_base_path with
	Some b -> ()
      | None -> begin
	  Printf.fprintf xml_file "<page>\n<title>%s</title>\n" title; 
	  Printf.fprintf xml_file "<id>%d</id>\n" page_id;
	end


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
      let comment = ml2str r#get_comment in
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
      let q1 = ml2str 
	(string_of__of__sexp_of sexp_of_qual_info_t quality_info_default) in 
      let q2 =  ml2float quality_info_default.reputation_gain in 
      let aq2 = if (q2 = "inf") then (ml2float infinity) else q2 in
      let q3 = ml2float quality_info.overall_trust in
      (* Db write access *)
      Printf.fprintf sql_file "INSERT INTO %swikitrust_revision (revision_id, page_id, text_id, time_string, user_id, username, is_minor, comment, quality_info, reputation_delta, overall_trust, wt_01, wt_23, wt_45, wt_67, wt_9, wt_9) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) ON DUPLICATE KEY UPDATE quality_info = %s, reputation_delta = %s, overall_trust = %s;\n"
	db_prefix rev_id page_id text_id time_string user_id username is_minor comment q1 aq2 q3 q1 aq2 q3
	

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
      let rep = rep_histories#get_rep uid rev_time in 
      let new_wl = rev#get_words in 
      (* Calls the function that analyzes the difference 
         between revisions. Data relative to the previous revision
         is stored in the instance fields chunks_a and chunks_attr_a *)
      let (new_chunks_a, medit_l) = Chdiff.text_tracking chunks_a new_wl in 

      (* Fixes the coefficients of trust incease depending on whether
	 the user is a bot... *)
      let (read_all', read_part') = 
	if Hashtbl.mem robots rev#get_user_name 
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
	  in
	  (read_all' *. time_factor, read_part' *. time_factor)
	end
      in
	
      (* Computes the trust, and the sigs *)
      let rep_float = float_of_int rep in 
      let (new_chunks_trust_a, new_chunks_sig_a) = 
	Compute_robust_trust.compute_robust_trust
	  chunks_trust_a chunks_sig_a new_chunks_a 
	  rev#get_seps medit_l rep_float uid 
	  trust_coeff_lends_rep trust_coeff_kill_decrease 
	  trust_coeff_cut_rep_radius read_all read_part 
	  trust_coeff_local_decay 
      in
      (* Computes the origin of the words in the new revision. *)
      let (new_chunks_origin_a, new_chunks_author_a) = 
	Compute_robust_trust.compute_origin 
	  chunks_origin_a chunks_author_a new_chunks_a medit_l 
	  rev#get_id rev#get_user_name 
      in 
      (* Replaces the chunks for the next iteration *)
      chunks_trust_a <- new_chunks_trust_a;
      chunks_origin_a <- new_chunks_origin_a; 
      chunks_author_a <- new_chunks_author_a;
      chunks_sig_a <- new_chunks_sig_a;
      chunks_a <- new_chunks_a;
      (* Also notes in the revision the reputations and the origins,
	 as well as the author_sigs. *)
      rev#set_word_trust new_chunks_trust_a.(0);
      rev#set_word_origin new_chunks_origin_a.(0);
      rev#set_word_author new_chunks_author_a.(0);
      rev#set_word_sig new_chunks_sig_a.(0);

      (* Outputs the colored text to the xml file, or to the filesystem,
	 as requested. *)
      begin
	match colored_base_path with 
	  (* Output to the xml file. *)
	  None -> rev#output_trust_origin_revision xml_file
	  (* Output to the filesystem. *)
	| Some b -> begin
	    let colored_text = rev#get_colored_text in
	    Filesystem_store.write_revision 
	      b rev#get_page_id rev#get_id colored_text
	  end
      end;

      (* Now we have to write the metadata for sql. *)
      self#write_wikitrust_revision_sql rev;


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
	disarmed_text false in 
      (* Adds the revision to the Vec of revisions. *)
      revs <- Vec.append r revs; 
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
    method private produce_page_information : unit =
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
      (* Produces page_info_t *)
      let page_info = {
	past_hi_rep_revs = [];
	past_hi_trust_revs = [];
      } in 
      let info_string_db = ml2str 
	(string_of__of__sexp_of sexp_of_page_info_t page_info) in 
      match sig_base_path with 
	None -> begin
	  let chunks_string_db = ml2str chunks_string in
	  Printf.fprintf sql_file "INSERT INTO %swikitrust_page (page_id, deleted_chunks, page_info) VALUES (%s, %s, %s) ON DUPLICATE KEY UPDATE deleted_chunks = %s, page_info = %s;\n" 
	    db_prefix (ml2int page_id) chunks_string_db info_string_db 
	    chunks_string_db info_string_db
	end
      | Some b -> begin
	  Printf.fprintf sql_file "INSERT INTO %swikitrust_page (page_id, page_info) VALUES (%s, %s) ON DUPLICATE KEY UPDATE page_info = %s;\n" 
	    db_prefix (ml2int page_id) info_string_db info_string_db;
	  Filesystem_store.write_revision b page_id 1 chunks_string
	  
	end

    (** This method stores the sigs of the last few revisions in the
	filesystem. *)
    method private output_sigs : unit = 
      (* f is folded on the Vec of revisions, producing a list of
	 (id, sig) that is ready to be written to disk *)
      let f r l = (r#get_id, r#get_sig) :: l in
      let sig_list : page_sig_disk_t = Vec.fold f revs [] in
      let sig_string = 
	string_of__of__sexp_of sexp_of_page_sig_disk_t sig_list in
      (* Writes the signature either into sql for the database, 
	 or into the filesystem. *)
      match sig_base_path with 
	Some b -> Filesystem_store.write_revision b page_id 0 sig_string
      | None -> begin
	  let s = ml2str sig_string in
	  Printf.fprintf sql_file 
	  "INSERT INTO %swikitrust_sigs (page_id, sigs) VALUES (%s, %s) ON DUPLICATE KEY UPDATE sigs = %s" 
	    db_prefix (ml2int page_id) s s
	end


    (** This method is called when there are no more revisions to evaluate. 
	We need to produce the sql that contains the page information. *)
    method eval: unit = 
      (* Produces the page information. *)
      self#produce_page_information;
      (* Produces the sig information to the filesystem. *)
      self#output_sigs;
      (* Closes the page in the xml file. *)
      match colored_base_path with
	Some b -> ()
      | None -> Printf.fprintf xml_file "</page>\n"
    
  end
