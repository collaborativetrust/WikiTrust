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

type word = string 
exception Ins_text_in_deleted_chunk
open Eval_defs

class page 
  (id: int)
  (title: string)
  (out_file: out_channel)
  (sql_file: out_channel)
  (colored_base_path: string)
  (rep_histories: Rephist.rephist)
  (trust_coeff_lends_rep: float)
  (trust_coeff_read_all: float) 
  (trust_coeff_read_part: float) 
  (trust_coeff_part_radius: float)
  (trust_coeff_cut_rep_radius: float) 
  (trust_coeff_kill_decrease: float)
  (n_rev_to_color: int) 
  (equate_anons: bool) 
  =

  (* TODO(Luca): We need a way to specify database table prefixes for the sql code. *)

object(self) 
    inherit Trust_local_color_analysis.page 
      id title out_file rep_histories
      trust_coeff_lends_rep trust_coeff_read_all 
      trust_coeff_read_part trust_coeff_part_radius
      trust_coeff_cut_rep_radius trust_coeff_kill_decrease
      n_rev_to_color equate_anons 

      (* This array keeps track of the revision id in which each word 
	 was introduced. *)
    val mutable chunks_origin_a : int array array = [| [| |] |]
      (* This array keeps track of the author of each word *)
    val mutable chunks_author_a : string array array = [| [| |] |]
      (* This array keeps track of the author sigs *)
    val mutable chunks_sig_a : Author_sig.packed_author_signature_t array array = [| [| |] |]


    (** Writes the SQL code for writing the wikitrust_revision to the db. *)
    method private write_wikitrust_revision_sql 
      (r: Revision.trust_revision) : unit = 
      (* Revision parameters *)
      (* ---qui--- fix: why store here the text_id? *)
      let rev_id = ml2int g#get_id in
      let page_id = ml2int r#get_page_id in
      let text_id = ml2int revision_info.rev_text_id in 
      let time_string = ml2str revision_info.rev_timestamp in 
      let user_id = ml2int revision_info.rev_user in 
      let username = ml2str revision_info.rev_user_text in 
      let is_minor = ml2int (if revision_info.rev_is_minor then 1 else 0) in 
      let comment = ml2str revision_info.rev_comment in 
      (* Quality parameters *)
      let q1 = ml2str (string_of__of__sexp_of sexp_of_qual_info_t quality_info) in 
      let q2 =  ml2float quality_info.reputation_gain in 
      let aq2 = if (q2 = "inf") then (ml2float infinity) else q2 in
      let q3 = ml2float quality_info.overall_trust in
      (* Db write access *)
      let s2 =  Printf.sprintf "INSERT INTO %swikitrust_revision (revision_id, page_id, text_id, time_string, user_id, username, is_minor, comment, quality_info, reputation_delta, overall_trust) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) ON DUPLICATE KEY UPDATE quality_info = %s, reputation_delta = %s, overall_trust = %s"
	db_prefix rev_id page_id text_id time_string user_id username is_minor comment q1 aq2 q3 q1 aq2 q3 in 



    (** Processes a new revision, computing trust, author, and origin, and outputting:
	- The colored revision text, compressed, in the filesystem.
	- The sql code for the online system metadata.
	- The revision metadata in an xml file, just in case. *)
    method private eval_newest : unit = 
      let rev_idx = (Vec.length revs) - 1 in 
      let rev = Vec.get rev_idx revs in 
      let uid = rev#get_user_id in 
      let t = rev#get_time in 
      (* Gets the reputation of the author of the current revision *)
      let rep = rep_histories#get_rep uid t in 
      let new_wl = rev#get_words in 
      (* Calls the function that analyzes the difference 
         between revisions. Data relative to the previous revision
         is stored in the instance fields chunks_a and chunks_attr_a *)
      let (new_chunks_a, medit_l) = Chdiff.text_tracking chunks_a new_wl in 
      (* Constructs new_chunks_attr_a, which contains the reputation range of the 
         author of each word in the text. *)
      let rep_float = float_of_int rep in 

      (* Computes the trust, and the sigs *)
      let (new_chunks_trust_a, new_chunks_sig_a) = Compute_robust_trust.compute_robust_trust
	chunks_trust_a chunks_sig_a new_chunks_a rev#get_seps medit_l rep_float uid 
	trust_coeff_lends_rep trust_coeff_kill_decrease trust_coeff_cut_rep_radius
	trust_coeff_read_all trust_coeff_read_part trust_coeff_local_decay in

      (* Computes the origin of the words in the new revision. *)
      let (new_chunks_origin_a, new_chunks_author_a) = Compute_robust_trust.compute_origin
	chunks_origin_a chunks_author_a new_chunks_a medit_l rev#get_id rev#get_user_name in 

      (* Replaces the chunks for the next iteration *)
      chunks_trust_a <- new_chunks_trust_a;
      chunks_origin_a <- new_chunks_origin_a; 
      chunks_author_a <- new_chunks_author_a;
      chunks_sig_a <- new_chunks_sig_a;
      chunks_a <- new_chunks_a;
      (* Also notes in the revision the reputations and the origins *)
      rev#set_word_trust new_chunks_trust_a.(0);
      rev#set_word_origin new_chunks_origin_a.(0);
      rev#set_word_author new_chunks_author_a(0);
      
      (* Outputs the colored text *)
      let colored_text = rev#get_colored_text in
      Filesystem_store.write_revision colored_base_path rev#get_page_id rev#get_id colored_text;

      (* Now we have to write the metadata for sql. *)

