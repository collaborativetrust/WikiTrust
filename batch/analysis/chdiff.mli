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

type edit = Ins of int * int | Del of int * int | Mov of int * int * int
type medit = Mins of int * int * int | Mdel of int * int * int | Mmov of int * int * int * int * int
type word = string
type index_t 
val print_chunks : word array array -> unit
val edit_distance : edit list -> int -> float
val make_index_diff : word array -> index_t
val edit_diff : word array -> word array -> index_t -> edit list
val text_survival :
  word array ->
  'a array ->
  word array list -> 
  'a array list ->
  word array ->
  (bool -> bool -> 'a -> 'a) ->
  'a -> 'a array * word array list * 'a array list
val text_tracking: word array array -> word array -> (word array array * medit list)
val zip_edit_lists : edit list -> edit list -> edit list
val diff_cover : edit list -> int * int 
val edit_diff_using_zipped_edits : word array -> word array -> edit list -> edit list -> edit list
val print_diff : edit list -> unit 
val print_mdiff : medit list -> unit
