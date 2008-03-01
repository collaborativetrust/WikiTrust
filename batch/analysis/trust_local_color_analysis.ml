(*

Copyright (c) 2007-2008 The Regents of the University of California
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


(** This class colorizes a wiki dump file according to text trust.
    Text trust is computed paying attention to syntactic text units,
    such as paragraphs and enumerations. *)

type word = string 
exception Ins_text_in_deleted_chunk
open Eval_constants

class page 
  (id: int)
  (title: string)
  (out_file: out_channel)
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
  (* Computes the geometrical decay of local trust. 
     Note the if-then-else to avoid divide by 0 *)
  let local_decay_coeff = 
    if trust_coeff_part_radius < 1. then 0. 
    else 
      (* local_decay_coeff ** trust_coeff_part_radius = 0.5 ; so... *)
      0.5 ** (1. /. trust_coeff_part_radius)
  in 

  object (self) 
    inherit Trust_analysis.page 
      id title out_file rep_histories
      trust_coeff_lends_rep trust_coeff_read_all 
      trust_coeff_cut_rep_radius trust_coeff_kill_decrease
      n_rev_to_color equate_anons 

    (** [compute_word_trust new_chunks_a medit_l rep_float] computes the 
        new word trust values of the revision with chunks [new_chunks_a], obtained from 
        the version with word trust values [chunks_trust_a] via the edit list [medit_l]. 
        [rep_float] is the reputation of the author of the new revision. 
        What is new about this method, compared to the classical compute_word_trust one, 
        is that this one takes into account syntactical units to spread the trust. *)
    method private compute_word_trust 
      (new_chunks_a: word array array) 
      (medit_l: Editlist.medit list) 
      (rep_float: float)
      (rev: Revision.trust_revision) : float array array =

      Compute_trust.compute_trust_chunks chunks_trust_a new_chunks_a rev#get_seps medit_l rep_float 
	trust_coeff_lends_rep trust_coeff_kill_decrease trust_coeff_cut_rep_radius 
	trust_coeff_read_all trust_coeff_read_part local_decay_coeff


  end (* page *)

