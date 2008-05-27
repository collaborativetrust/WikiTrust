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
    online WikiTrust implementation. *)

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
  (** This is the revision_id where each word of the text of these 
      deleted chunks was first introduced. *)
  origin: int array;
}

(** These are the coefficients used for the evaluation. *)
type trust_coeff_t = {
  (** Number of revision to use for trust computation *)
  mutable n_revs_to_consider : int; 
  (** Max time a chunk can be deleted before it is discarded *)
  mutable max_del_time_chunk : float; 
  (** max n. of revisions for which a chunk can be deleted before being discarded *)
  mutable max_del_revs_chunk : int; 
  (** how much reputation is lent as trust for new text *)
  mutable lends_rep : float; 
  (** how much the text of revised articles raises in trust towards the 
      reputation of the editor *)
  mutable read_all : float; 
  (** how much the text of revised articles, in the portion of article directly edited, 
      raises in trust towards the reputation of the editor *)
  mutable read_part: float; 
  (** how much the trust of text is lost when text is deleted *)
  mutable kill_decrease: float; 
  (** how much trust propagates from the edges of block moves *)
  mutable cut_rep_radius: float; 
  (** the text of revised articles that is local to an edit increases more in trust
      when revised (see read_part).  This coefficient says how fast this "locality" 
      effect decays at the border of a local area, into the non-local area.  A value
      of 0 is perfectly fine. *)
  mutable local_decay: float; 
  (** scaling for reputation increments *)
  mutable rep_scaling: float; 
  (** maximum reputation *)
  mutable max_rep: float;
  (** Whether to equate anonymous users, regardless of their IP. *)
  mutable equate_anons: bool; 
  (** Interval of time for nixing *)
  mutable nix_interval: float; 
}

let default_trust_coeff = {
  n_revs_to_consider = 20;
  max_del_time_chunk = 30. *. 24. *. 3600.; (* 2 months *)
  max_del_revs_chunk = 100;
  lends_rep = 0.4;
  read_all = 0.2;
  read_part = 0.2;
  kill_decrease = (log 2.0) /. 9.0;
  cut_rep_radius = 4.0;
  local_decay = 2.0; 
  rep_scaling = 75.0;
  max_rep = 22026.465795 -. 2.0;
  equate_anons = false;
  nix_interval = 24. *. 3600.
}
 
let get_default_coeff : trust_coeff_t = default_trust_coeff ;;

(** This is the quality information we store with revisions *)
type quality_info_t = {
  (** Number of times the revision has been judged *)
  mutable n_edit_judges: int; 
  (** Total edit quality: the average is given by dividing by n_edit_judges *)
  mutable total_edit_quality: float;
  (** Minimum edit quality of all judgements *)
  mutable min_edit_quality: float; 
  (** Nix bit (see the techrep) *)
  mutable nix_bit: bool;
}
