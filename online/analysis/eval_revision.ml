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

(* eval_revision.ml 
   This module is responsible for coloring a revision according to trust, and 
   for computing the effect of the revision on author reputations. *)

(** Number of past revisions to try to analyze *)
let n_past_revisions_to_consider = 12

let eval (rev_id: int) (page_id: int): unit = 
(*
  Procedure to be followed: 

  - Fish out the previous n_past_rev revisions. 

  - Compare the new revision to the previous ones, determining the edit lists
    and edit distances. 

  - Do the algorithm based on triangulation for assigning edit reputation. 

  - Let k2 be the revision closest in edit distance to the most recent one, 
    and let k1 be the revision immediately preceding the most recent one, 
    which we call k0. 

  - For text tracking, determine how the text of the page would have been 
    tracked in k2 -> k0.  Note the words that appear new in k0. 
    Next, do the analysis k1 -> k0. 
    Then, use: 
    For the live chunk, the results by k2 -> k0, with any place that is marked as
    new in k0, taking the source index from k1 -> k0. 
    The dead chunks are as in k1 -> k0. 
    
  - For trust, compute the trust resulting from k2 -> k0, and from k1 -> k0, and
    use for each word the maximum of the trust values resulting from these two 
    derivations. 

  - Store trust values, origin, deleted chunks, and edit lists back into the db.

 *)
  (* First, we make a page object, through which we will access the database. *)
  let p = new Online_page.page page_id in 
  (* Now, I must build a Vec of past revisions. *)
  let revs = build_rev_vec p n_past_revisions_to_consider in 
