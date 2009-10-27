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

(** This file contains types that are used by several modules of the 
    online WikiTrust implementation.  It belongs to the batch implementation
    because the batch trust analysis needs to be able to prepare data 
    in the format used by the online analysis. *)

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"    

(** Type of an author in the annotated text.  Choose int if you wish to 
    annotate text with author ids, and string if you wish to annotate 
    with author names. *)
type author_t = string with sexp

(** A chunk is a portion of text that used to be part of an article, but that 
    has since been deleted.  We associate a chunk list with each page. *)
type chunk_t = {
  (** The timestamp is the time at which the chunk was deleted from the page. 
      This is to make it possible to delete chunks that have been deleted for 
      very long (otherwise, they could accumulate). *)
  mutable timestamp: float; 
  (** Number of revisions for which a chunk has been deleted. 
      The purpose is similar to above *)
  mutable n_del_revisions: int; 
  (** This is the array of words.  Note that we store the words, not the 
      seps.  This because all we need to know of the deleted chunks is if they 
      are re-inserted, via text comparison, which is based on words. *)
  text: string array; 
  (** This is the trust of the text that has been deleted. *)
  trust: float array;
  (** These are the author signatures for the trust *)
  sigs: Author_sig.packed_author_signature_t array;
  (** This is the revision_id where each word of the text of these 
      deleted chunks was first introduced. *)
  origin: int array;
  (** This is the author of each word *)
  author: author_t array;
} with sexp

(** This type represents a signature *)
type sig_t = {
  words_a: string array;
  trust_a: float array;
  origin_a: int array;
  author_a: string array;
  sig_a: Author_sig.packed_author_signature_t array;
} with sexp

(* This is the internal type for sets of signatures, which can be serialized *)
type page_sig_disk_t = (int * sig_t) list with sexp

(** These are the coefficients used for the evaluation. *)
type trust_coeff_t = {
  (** Number of revision to use for trust computation *)
  mutable n_revs_to_consider : int; 
  (** Length of list of previous high reputation versions of the page *)
  mutable len_hi_rep_revs: int; 
  (** Length of list of previous high trust versions of the page *)
  mutable len_hi_trust_revs: int; 
  (** Threshold of reputation for an author to be included in a
      hi-reputation list *)
  mutable hi_rep_list_threshold: float;
  (** Max time a chunk can be deleted before it is discarded *)
  mutable max_del_time_chunk : float; 
  (** max n. of revisions for which a chunk can be deleted before
      being discarded *)
  mutable max_del_revs_chunk : int; 
  (** Max n. of words in a deleted chunk (if longer, it is truncated) *)
  mutable max_dead_chunk_len : int;
  (** how much reputation is lent as trust for new text *)
  mutable lends_rep : float; 
  (** how much the text of revised articles raises in trust towards the 
      reputation of the editor *)
  mutable read_all : float; 
  (** how much the text of revised articles, in the portion of article
      directly edited, raises in trust towards the reputation of the
      editor *)
  mutable read_part: float; 
  (** how much the trust of text is lost when text is deleted *)
  mutable kill_decrease: float; 
  (** how much trust propagates from the edges of block moves *)
  mutable cut_rep_radius: float; 
  (** The text of revised articles that is local to an edit increases
      more in trust when revised (see read_part).  This coefficient
      says how fast this "locality" effect decays at the border of a
      local area, into the non-local area.  A value of 0 is perfectly
      fine. *)
  mutable local_decay: float; 
  (** scaling for reputation increments *)
  mutable rep_scaling: float; 
  (** a function which returns a value based on how mature the page is. *)
  mutable dynamic_rep_scaling: int -> int -> float;
  (** maximum reputation *)
  mutable max_rep: float;
  (** Whether to equate anonymous users, regardless of their IP. *)
  mutable equate_anons: bool; 
  (** Interval of time for nixing *)
  mutable nix_interval: float; 
  (** Negative quality below which nixing happens *)
  mutable nix_threshold: float;
  (** The high-median of the reputations is used for the white value. 
   We choose it so that 90% of work is done below that value. *)
  mutable hi_median_perc: float;
  (** This is a similar median, but is used to renormalize the weights of 
      authors during the initial phase of a wiki *)
  mutable hi_median_perc_boost: float;
  (** This is the characteristic time, in seconds, of an edit.
      If the time is much shorter than this, the effect on text trust is
      proportionately diminished; if the characteristic time is greater, 
      the effect on text trust is the full effect. *)
  mutable edit_time_constant: float
};;

(* Number of past revisions to consider *)
let n_past_revs = 8;;

(* We compute the reputation scaling dynamically taking care of the
   size of the recent_revision list and the union of the recent
   revision list, hig reputation list and high trust list *)
let default_dynamic_rep_scaling n_recent_revs max_n_recent_revs = 
  let n_revs_judged = max 1 (min (n_recent_revs - 2) (max_n_recent_revs / 2)) in 
  1. /. (float_of_int n_revs_judged)

let default_trust_coeff = {
  n_revs_to_consider = n_past_revs;
  len_hi_trust_revs = 2;
  len_hi_rep_revs = 2;
  hi_rep_list_threshold = 6.0;
  max_del_time_chunk = 90. *. 24. *. 3600.; (* 3 months *)
  max_del_revs_chunk = 100;
  max_dead_chunk_len = 10000;
  lends_rep = 0.3;
  read_all = 0.10;
  read_part = 0.18;
  kill_decrease = (log 2.0) /. 9.0;
  cut_rep_radius = 2.0;
  local_decay = 0.5 ** (1. /. 10.); 
  (* The reputation scaling is 73.24 when we use n_revs_to_consider = 12, 
     and varies quadratically with n_revs_to_consider - 1. *)
  rep_scaling = 1. /. (73.24 *. ( ((float_of_int n_past_revs) -. 1.) /. 11.) ** 2.);
  dynamic_rep_scaling = default_dynamic_rep_scaling;
  max_rep = 22026.465795 -. 2.0;
  equate_anons = false;
  nix_interval = 24. *. 3600.;
  nix_threshold = 0.1;
  hi_median_perc = 0.9;
  hi_median_perc_boost = 0.7;
  edit_time_constant = 24. *. 60. *. 60.; (* 1 day *)
};;
 
let get_default_coeff : trust_coeff_t = default_trust_coeff ;;

(** This is the quality information we store with revisions *)
type qual_info_t = {
  (** Number of times the revision has been judged *)
  mutable n_edit_judges: int; 
  (** Total edit quality: the average is given by dividing by n_edit_judges *)
  mutable total_edit_quality: float;
  (** Minimum edit quality of all judgements *)
  mutable min_edit_quality: float; 
  (** Nix bit (see the techrep) *)
  mutable nix_bit: bool;
  (** Delta, or the amount of change done *)
  mutable delta: float;
  (** Total reputation accrued due to the revision *)
  mutable reputation_gain: float;
  (** Overall trust of a revision *)
  mutable overall_trust: float;
  (** Histogram of word trust *)
  mutable word_trust_histogram: int array;
} with sexp

(* Infinity for reputation delta *)
let infinity = 100000.0;;

(* Default for quality information.  This is used both for
   new revisions, and for revisions that have been processed
   as part of the online analysis. *)
let quality_info_default = {
  n_edit_judges = 0;
  total_edit_quality = 0.;
  min_edit_quality = 1000.;
  nix_bit = false;
  delta = 0.0;
  reputation_gain = 0.0;
  overall_trust = 0.0;
  word_trust_histogram = Array.make 10 0
}

(** This is the type of an edit list, annotated *)
type edit_list_t = {
  (** version of text analysis algo *)
  split_version : string; 
  (** to which version *)
  to_version : int; 
  (** the edit list proper *)
  editlist : Editlist.edit list 
} with sexp

type edit_lists_of_rev_t = edit_list_t list with sexp

(** This is the information associated with a page *)
type page_info_t = { 
  (** List of revision by hi rep authors: 
      list of (rev_id, user_id, author_rep) triples *)
  mutable past_hi_rep_revs : (int * int * float) list;
  (** List of revision with high trust: list of (rev_id, rev_trust) pairs *)
  mutable past_hi_trust_revs : (int * float) list; 
} with sexp 

let page_info_default = { 
  past_hi_rep_revs = [];
  past_hi_trust_revs = [];
}

(* Timestamp in the DB *)
type timestamp_t = int * int * int * int * int * int;;

(* Parameters for blob storage. *)
type blob_params_t = {
  (* Minimum uncompressed blob size *)
  min_blob_size: int;
  (* Maximum uncompressed blob size *)
  max_blob_size: int;
  (* Max n. of revisions per blob once the blob is over the min size. *)
  max_revs_per_blob: int;
}

let blob_params = {
  min_blob_size =  1000000;
  max_blob_size = 10000000;
  max_revs_per_blob = 20;
}

(* Sigs and deleted chunks are stored in a very specific position. *)
type blob_locations_t = {
  invalid_location: int;
  sig_location: int;
  chunks_location: int;
  initial_location: int;
}

let blob_locations = {
  invalid_location = 0; (* A revision sig can never be here. *)
  sig_location = 0;
  chunks_location = 1;
  (* I use 8 as an initial location to leave a bit of space for other
     information we may wish to compress. *)
  initial_location = 8;
}

(* Things for the WMF implementation *)

type request_type_t = Vote | Coloring

(* Types for talking with Wikipedia *)
type wiki_page_t = {
  page_id : int;
  page_namespace : int;
  page_title : string; 
  page_restrictions : string;
  page_counter : int;
  page_is_redirect : bool;
  page_is_new : bool;
  page_random : float;
  page_touched : string; 
  page_latest : int;
  page_len : int
} with sexp

type wiki_revision_t = {
  revision_id : int;
  mutable revision_page : int;
  revision_text_id : int;
  revision_comment : string;
  mutable revision_user : int;
  revision_user_text : string;
  revision_timestamp : string;
  revision_minor_edit : bool;
  revision_deleted : bool;
  revision_len : int;
  revision_parent_id : int;
  revision_content : string;
} with sexp

(** High-m%-Median of an array *)
let compute_hi_median (a: float array) (m: float) =
  let total = Array.fold_left (+.) 0. a in 
  let mass_below = ref (total *. m) in 
  let median = ref 0. in 
  let i = ref 0 in 
  while (!mass_below > 0.) && (!i < Eval_defs.max_rep_val) do begin 
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
  !median

let compute_reputation_median a = 
  compute_hi_median a default_trust_coeff.hi_median_perc

(* Decides whether a user is a robot, checking against a hashtable,
   but also whether its name ends by "bot". *)
let is_user_a_bot robots username =
  (Hashtbl.mem robots username) ||
    (let l = String.length username in
    l >= 3 && (
      let s = String.lowercase username in
      let e = String.sub s (l - 3) 3 in
      e = "bot"))
