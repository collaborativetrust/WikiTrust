(*

Copyright (c) 2009 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye

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

exception API_error of string;;
exception API_error_noretry of string;;

type selector_t =
  | Title_Selector of string
  | Page_Selector of int
  | Rev_Selector of int

(* 19700201000000 *) 
val default_timestamp : string

(** [get_remote_user_id user_name] returns the user_id of user with name [user_name]. 
    This involves querying the toolserver, which is usaually heavily loaded,
    resulting in long response times.
 *)
val get_remote_user_id : string -> int

(** Downloads all revisions of a page, given the page_id, and sticks them into the db, returning the number of revs downloaded. *)
val download_page_from_id : Online_db.db -> int -> int

val download_page_starting_with_from_id : Online_db.db -> int -> int -> int 
  -> int -> int
(** Downloads all revisions of a page, given the page_title, and sticks them into the db. *)
val download_page_starting_with : Online_db.db -> string -> int -> int -> unit

(** Reads a group of rev_lim revisions from the WpAPI and sticks them in the db. *)
val get_revs_from_api : selector_t -> int -> int ->
    (Online_types.wiki_page_t * Online_types.wiki_revision_t list * int option)

(**
  Render the html using the wikimedia api
*)
val render_revision : string -> string
