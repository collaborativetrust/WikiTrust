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

(** Type of packed signature types. *)
type packed_author_signature_t

(** Type of the signature of a single author *)
type author_signature_t

(** Empty signatures: no author has yet signed. *)
val empty_sigs : packed_author_signature_t

(** [is_author_in_sigs id w sigs] returns [true] if author [id] is in the signatures [sigs] of 
    word [w], and returns [false] otherwise. *)
val is_author_in_sigs : int -> string -> packed_author_signature_t -> bool

(** [add_author id word sigs] adds author id to the signatures [sigs] for word [word], 
    and returns the new signature.  It assumes that the author was not already in the 
    list. *)
val add_author :
  int -> string -> packed_author_signature_t -> packed_author_signature_t

(** [compute_auth_sig id w] computes the signature for author [id] and word [w]. *)
val compute_auth_sig : int -> string -> author_signature_t

(** [is_sig_in_sigs s sigs] returns [true] if the signature [s] is in [sigs], and
    [false] otherwise. *)
val is_sig_in_sigs : author_signature_t -> packed_author_signature_t -> bool

(** [add_sig_to_sigs s sigs] adds the signature [s] at the beginning of the signatures 
    [sigs], and returns the resulting packed signature. *)
val add_sig_to_sigs :
  author_signature_t -> packed_author_signature_t -> packed_author_signature_t
