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

