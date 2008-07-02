(*

Copyright (c) 2008 The Regents of the University of California
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

(** This class provides a handle for accessing the database in the on-line 
    implementation. *)

open Online_types;;

exception DB_Not_Found;;

class db : 
  string (* user *) -> 
  string (* auth *) ->
  string (* database name *) -> 

  object
   
    method print_stats : unit
    (** [fetch_last_colored_rev : (rev_id, page_id)]] Returns the revid and page
        id of the last colored revision. Raises DB_Not_Found if no revisions have been colored.
    *)    
    method fetch_last_colored_rev : (int * int * (int * int * int * int * int * int))
    
    (** [sth_select_all_revs_after (int * int * int * int * int * int)] returns all 
        revs created after the given timestamp. *)
    method fetch_all_revs_after : (int * int * int * int * int * int) -> Mysql.result

    (** [fetch_all_revs] Returns a pointer to a result set consisting in all the 
	revisions of the database, in ascending temporal order. *)
    method fetch_all_revs : Mysql.result

    (** [fetch_revs pageid revid] Returns a pointer to a result set of revisions for a given 
        page, starting at rev_id, and going back in time. 
    *)
    method fetch_revs : int -> int -> Mysql.result 
    (** [read_edit_diff revid1 revid2] reads from the database the edit list 
	from the (live) text of revision [revid1] to revision [revid2]. 
        The edit list consists in a string, identifying the way revision of 
        Text.ml used for splitting the text, and in the edit list proper.
        The return type is an option: the db should return None if no such row for 
        revid1 and revid2 can be found in the database. *)
    method read_edit_diff : int -> int -> (string * (Editlist.edit list)) option  

    (** [write_edit_diff revid1 revid2 vers elist] writes to the database the edit list 
	[elist] from the (live) text of revision [revid1] to revision [revid2], 
	computed by splitting the text of revision with version [vers] of 
	Text.ml.  Note that for each revid1 and revid2, I want a UNIQUE [vers] and [elist]; 
	previous values should be over-written. *)
    method write_edit_diff : int -> int -> string -> (Editlist.edit list) -> unit

    (** [get_rev_text text_id] returns the text associated with text id [text_id] *)
    method read_rev_text : int -> string 

    (** [get_rep uid] gets the reputation of user [uid], from a table 
	relating user ids to their reputation *)
    method get_rep : int -> float

    (** [set_rep uid r] sets, in the table relating user ids to reputations, 
	the reputation of user [uid] to be equal to [r]. *)
    method set_rep : int -> float -> unit

    (** [set_rep_hist uid t r0 r1] writes, in a table with keys user_id, time, 
	and reputation, that at time [t] the reputation of user [uid] went from
	[r0] to [r1]. *)
    method set_rep_hist : int -> float -> float -> float -> unit

    (** [write_colored_markup rev_id markup] writes, in a table with columns by 
	(revision id, string), that the string [markup] is associated with the 
	revision with id [rev_id]. 
	The [markup] represents the main text of the revision, annotated with trust 
	and origin information; it is what the "colored revisions" of our 
	batch demo are. 
	When visitors want the "colored" version of a wiki page, it is this chunk 
	they want to see.  Therefore, it is very important that this chunk is 
	easy and efficient to read.  A filesystem implementation, for small wikis, 
	may be highly advisable. *)
    method write_colored_markup : int -> string -> unit 

    (** [read_colored_markup rev_id] reads the text markup of a revision with id
	[rev_id].  The markup is the text of the revision, annontated with trust
	and origin information. *)
    method read_colored_markup : int -> string

    (** [write_dead_page_chunks page_id chunk_list] writes, in a table indexed by 
	(page id, string list) that the page with id [page_id] is associated 
	with the "dead" strings of text [chunk1], [chunk2], ..., where
	[chunk_list = [chunk1, chunk2, ...] ]. 
	The chunk_list contains text that used to be present in the article, but has 
	been deleted; the database records its existence. *)
    method write_dead_page_chunks : int -> Online_types.chunk_t list -> unit

    (** [read_dead_page_chunks page_id] returns the list of dead chunks associated
	with the page [page_id]. *)
    method read_dead_page_chunks : int -> Online_types.chunk_t list

    (** [write_quality_info rev_id qual_info] writes that the revision with 
	id [rev_id] has quality as described in [qual_info]. *)
    method write_quality_info : int -> qual_info_t -> unit 

    (** [read_quality_info rev_id] returns a record of type quality_info_t 
	containing quality information for a revision *)
    method read_quality_info : int -> qual_info_t option

    (** [get_page_lock page_id] gets a lock for page [page_id], to guarantee 
	mutual exclusion on the updates for page [page_id]. *)
    method get_page_lock : int -> unit

    (** [release_page_lock page_id] releases the lock for page [page_id], to guarantee 
	mutual exclusion on the updates for page [page_id]. *)
    method release_page_lock : int -> unit

    (** [get_rep_lock] gets a lock for the global table of user reputations, to guarantee 
	serializability of the updates. *)
    method get_rep_lock : unit

    (** [release_rep_lock] releases a lock for the global table of user reputations, to guarantee 
	serializability of the updates. *)
    method release_rep_lock : unit

    (** Totally clear out the db structure -- THIS IS INTENDED ONLY FOR UNIT
    TESTING *)
    method delete_all : bool -> unit

  end

