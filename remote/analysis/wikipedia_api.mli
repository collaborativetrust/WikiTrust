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

(* 19700201000000 *) 
val default_timestamp : string

(**
   [fetch_page_and_revs after page_title rev_date], given a [page_title] 
   and a [rev_date], returns [rev_lim] revisions of [page_title] after [rev_date]. 
   The return value, in detail, consists of: 
   * A page option, containing the page information.  This is present
     if anything else is present.
   * A list of revision metadata.
   * The id of the next revision, if known.
   See http://en.wikipedia.org/w/api.php for more details.
*)
val fetch_page_and_revs_after : string ->
  (Online_types.wiki_page_t option * 
			  Online_types.wiki_revision_t list * int option) 

(** [get_user_id user_name] returns the user_id of user with name [user_name]. 
    This involves querying the toolserver, which is usaually heavily loaded,
    resulting in long response times.
 *)
val get_user_id : string -> Online_db.db -> int

(**
   [get_revs_from_api page_title last_timestamp db rev_lim] reads 
   a group of rev_lim revisions of the given page from the Wikimedia API,
   stores them to disk, and returns:
   - an optional id of the next revision to read.  If None, then
     all revisions of the page have been read.
   Raises API_error if the API is unreachable.
*)
val get_revs_from_api : string -> int -> 
    Online_db.db -> int ->
    int option

(** Downloads all revisions of a page, given the title, and sticks them into the db. *)
val download_page : Online_db.db -> string -> unit

val download_page_starting_with : Online_db.db -> string -> int -> unit

val get_revs_from_pageid : int -> int -> int ->
    (Online_types.wiki_page_t option * Online_types.wiki_revision_t list * int option)
val get_rev_from_revid : int ->
    (Online_types.wiki_page_t option * Online_types.wiki_revision_t list * int option)

(**
  Render the html using the wikimedia api
*)
val fetch_rev_api : string -> string
