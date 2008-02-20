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


type word = string
(** The type of words *)

type sep_t =
    (** The type of word tokens *)
    Title_start of string
      (** start sequence for a title *)
  | Title_end of string
      (** end sequence for a title *)
  | Par_break of string
      (** paragraph break sequence *)
  | Bullet of string
      (** bullet sequence *)
  | Indent of string
      (** indentation sequence *)
  | Space of string
      (** normal whitespace, without a newline *)
  | Newline of string 
      (** whitespace containing a newline char *)
  | Armored_char of string 
      (** Armored char such as &nbsp; *)
  | Table_line of string 
      (** table tag that needs to be alone on a line *)
  | Table_cell of string 
      (** table tag that signals the start of a cell *)
  | Table_caption of string 
      (** table tag for the caption *)
  | Tag of string * int
      (** tag, along with the position in the word array *)
  | Word of string * int
      (** normal word, along with the position in the word array *)
  | Redirect of string * int
      (** redirection tag, along with the position in the word array *)

val split_into_words : string Vec.t -> word array
  (** [split_into_words sv] splits a Vec of strings [sv] into an array of words.
      Used for reputation analysis. *)

val split_into_words_and_seps : string Vec.t -> ((word array) * (sep_t array) * (int array))
  (** [split_into_words_and_seps sv] splits a Vec of strings [sv] into an array of words, 
      and into an array of word+separators, as well as in an array that gives, for each
      word, the position in the array of separators. *)

val print_words : word array -> unit
  (** [print_words wa] prints the words in the word array [wa]. *)

val print_seps : sep_t array -> unit 
  (** [print_seps wa] prints the array of separators [wa] *)

val print_words_and_seps : (word array) -> (sep_t array) -> unit
  (** [print_words_and_seps ws sp] prints the array of words [ws] and the array of separators [wa] *)


