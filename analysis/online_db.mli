(*

Copyright (c) 2008-2010 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye, B. Thomas Adler

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

(* Returned whenever something is not found *)
exception DB_Not_Found
(* Internal error *)
exception DB_Internal_Error
(* Commit failed, or other database error that may have to cause a rollback. *)
exception DB_TXN_Bad
(* ExecAPI failed *)
exception DB_Exec_Error
(* Read/write colored revisions from wrong blob ids. *)
exception DB_Illegal_blob_id

(** Represents the revision table in memory *)
type revision_t = {
  rev_id: int; 
  rev_page: int; 
  rev_text_id: int; 
  rev_timestamp: string; 
  rev_user: int; 
  rev_user_text: string;
  rev_is_minor: bool; 
} 

(** This is the type of a vote data *)
type vote_t = {
  vote_time: string;
  vote_page_id: int; 
  vote_revision_id: int;
  vote_voter_name: string;
}

(** This is the type of a signature set *)
type page_sig_t
val empty_page_sigs : page_sig_t

(* If a signatures base path is provided, then we store: 
   At position 0: the sigs of the page.
   At position 1: the deleted chunks of the page. *)
class db : 
  string ->           (* db prefix *)
  Mysql.dbd ->        (* mediawiki db handle *)
  Mysql.dbd option -> (* global (interwiki) db handle *)
  string ->           (* database name *)
  string option ->    (* revision base path *)
  string option ->    (* colored revisions base path *)
  bool ->             (* debug_mode *)
  bool ->             (* keep text cache *)
  object

    (* ================================================================ *)
    (* General methods *)
    (* Gets the colored base path. *)
    method get_base_path : string option

    (* ================================================================ *)
    (* Disconnect *)
    method close : unit 

    (* ================================================================ *)
    (* Locks. *)

    (** [get_page_lock page_id timeout] gets a lock for page
	[page_id], to guarantee mutual exclusion on the updates for
	page [page_id].  The lock is waited for at most time [timeout]
	seconds.  The function returns [true] if the lock was
	acquired. *)
    method get_page_lock : int -> int -> bool

    (** [is_page_lock_free page_id] checks whether the lock for page
	[page_id] is available (there is no guarantee that somebody
	does not lock the page between this test and a subsequent call
	to [get_page_lock]. *)
    method is_page_lock_free : int -> bool

    (** [release_page_lock page_id] releases the lock for page
	[page_id], to guarantee mutual exclusion on the updates for
	page [page_id]. *)
    method release_page_lock : int -> unit

    (** Start a transaction *)
    method start_transaction : unit

    (** Rollback a transaction *)
    method rollback_transaction : unit 

    (** Commit of transaction *)
    method commit : unit

    (* ================================================================ *)
    (* Global methods. *)

    (** [get_histogram] Returns a histogram showing the number of users 
	at each reputation level *)
    method get_histogram : float array * float
    
    (** write_histogram delta_hist median] increments the db histogram
	of user reputations according to the array [delta_hist], and
	writes that the new median is [median]. *)
    method write_histogram : float array -> float -> unit 

    (** set_histogram hist median] sets the histogram and the median in the
	database. *)
    method set_histogram : float array -> float -> unit 

    (** [fetch_last_colored_rev_time req_page_id] returns the timestamp and the 
	revision id of the most recent revision that has been colored.
	If [req_page_id] specifies a page, then only that page is considered.
        Raises DB_Not_Found if no revisions have been colored. *)    
    method fetch_last_colored_rev_time : int option -> string * int
    
    (** [fetch_all_revs_after req_page_id req_rev_id timestamp rev_id
        limit] returns all revs created after the given [timestamp],
        or at the same [timestamp], with revision id at least
        [rev_id], up to the maximim number [limit].  If [req_page_id]
        specifies a page, then only that page is considered.  If
        [req_rev_id] specifies a revision, then that revision is
        included in the result. *)
    method fetch_all_revs_after : int option -> int option -> string -> int -> int -> revision_t list

    (** [fetch_all_revs req_page_id max_revs_to_return] returns a list
	of revisions in the database, in ascending order of timestamp,
	of length at most [max_revs_to_return].  If [req_page_id]
	specifies a page, then only that page is considered. *)
    method fetch_all_revs : int option -> int -> revision_t list

    (** [fetch_unprocessed_votes req_page_id n_events] returns at most
	[n_events] unprocessed votes, starting from the oldest
	unprocessed vote.  If [req_page_id] specifies a page, then
	only that page is considered. *)
    method fetch_unprocessed_votes : int option -> int -> vote_t list

    (** [mark_vote_as_processed (revision_id: int) (voter_name : string)]
	marks a vote as processed. *)
    method mark_vote_as_processed : int -> string -> unit


    (* ================================================================ *)
    (* Blob methods. *)

    (** [read_blob page_id blob_id] reads the blob for page_id and blob_id,
	either from the database, or from the filesystem, and returns it. *)
    method read_blob : int -> int -> string option

    (** [write_blob page_id blob_id blob_content] writes the blob [blob_content]
	for [page_id], [blob_id] to either the filesystem or the database. *)
    method write_blob : int -> int -> string -> unit 


    (* ================================================================ *)
    (* Page methods.  We assume we have a lock on the page when calling
       these methods. *)

    (* Methods for wikitrust_page *)

    (** [init_page page_id] initializes the page information for 
	page [page_id]. *)
    method init_page : int -> string option -> unit

    (** [write_page_info page_id p_info] writes that the page [page_id]
	has associated information [p_info].  The list of deleted chunks
	of the page is not modified. *)
    method write_page_info : int -> Online_types.page_info_t -> unit

  (** [read_page_info page_id] returns the page information for [page_id],
      consisting of the page_info and the open_blob_id. *)
    method read_page_info : int -> Online_types.page_info_t * int

    (** [fetch_col_revs page_id timestamp rev_id fetch_limit] returns a
	cursor that points to at most [fetch_limit] colored revisions of page
	[page_id] with time and revision immediately preceding 
        [timestamp], and [rev_id]. *)
    method fetch_col_revs : int -> timestamp_t -> int -> int -> Mysql.result

    (** [get_mwpage_title page_id]
	Returns the (list) of titles associated with page_id
	in the MediaWiki tables *)
    method get_mwpage_title : int -> string list
    (** [get_mwpage_id page_title]
	Returns the (list) of page_ids associated with page_title
	in the MediaWiki tables *)
    method get_mwpage_id : string -> int list

    (** [delete_page page_id delete_also_mediawiki] deletes all the information related to [page_id]
	in the system.  If [delete_also_mediawiki] is true, deletes also the info on the page
        in the normal Mediawiki tables. *)
    method delete_page : int -> bool -> unit


    (* Chunk methods *)

    (** [write_page_chunks page_id chunk_list] writes that the page
	with id [page_id] is associated with the "dead" strings of text
	[chunk1], [chunk2], ..., where [chunk_list = [chunk1, chunk2,
	...] ].  The chunk_list contains text that used to be present in
	the article, but has been deleted.  *)
    method write_page_chunks : int -> (chunk_t list) -> unit
      
  (** [read_page_chunks page_id] returns the chunk list for page [page_id]. 
      If the chunks cannot be found, returns the empty list: this happens
      for new pages. *)
    method read_page_chunks : int -> chunk_t list

  (** [write_open_blob_id page_id blob_id] writes on the wikitrust_page table that
      the open blob for [page_id] is [blob_id]. *)
  method write_open_blob_id : int -> int -> unit


    (* Signature methods. *)

    (** [read_page_sigs page_id] reads and returns the sigs for page
	[page_id]. *)
    method read_page_sigs : int -> page_sig_t

    (** [write_page_sigs page_id sigs] writes that the sigs 
	page [page_id] are [sigs]. *)
    method write_page_sigs : int -> page_sig_t -> unit

    (* Methods on the standard tables. *)

    (** [get_latest_rev_id_from_id page_id] returns the revision id of the 
        most recent revision of page [page_id]. *)
    method get_latest_rev_id_from_id : int -> int

    (* ================================================================ *)
    (* Revision methods.  We assume we have a lock on the page to which 
       the revision belongs when calling these methods. *)

  (** [read_wikitrust_revision rev_id] reads a revision from the 
      wikitrust_revision table, returning the revision information,
      the quality information, and the optional blob id. *)
    method read_wikitrust_revision : 
      int -> (revision_t * qual_info_t * int option)

    (** [write_wikitrust_revision revision_info quality_info blob_id_opt]
	writes the wikitrust data associated with a revision. *)
    method write_wikitrust_revision : 
      revision_t -> qual_info_t -> int option -> unit

    (** [read_revision_quality rev_id] reads the wikitrust quality
	information of revision_id *)
    method read_revision_quality : int -> (qual_info_t * int option)

  (** [write_colored_markup page_id rev_id blob_id_opt page_open_blob markup] 
      writes the "colored" text [markup] of a revision.  The [markup]
      represents the main text of the revision, annotated with trust
      and origin information. [page_id] and [rev_id] are as usual. 
      [blob_id_opt] specifies the blob in which the information should
      be written, if known.  Otherwise, the information is written in 
      [page_open_blob] blob.  The function returns a pair, consisting of:
      - The blob in which the revision was written 
        (this coincides with the content of [blob_id_opt] when the latter 
        is not null). 
      - The new open blob for the page (this may coincide with the old
        open blob, of course). *)
    method write_colored_markup :
      int -> int -> int option -> int -> string -> (int * int)

    (** [read_colored_markup rev_id blob_id_opt] reads the text markup
	of a revision with id [rev_id].  The markup is the text of the
	revision, annotated with trust and origin information. 
	The revision can be found in [blob_id_opt] (if not None). 
	If the revision cannot be found, including if [blob_id_opt] is None, 
	the DB_Not_Found exception is raised. *)
    method read_colored_markup : int -> int -> int option -> string

  (** [write_trust_origin_sigs page_id rev_id page_sigs words trust
      origin sigs] writes that the revision [rev_id] is associated
      with [words], [trust], [origin], and [author_sigs].  The
      function is given the page signatures [page_sigs], and does not
      actually write things to disk; rather, it updates these
      page_sigs in-place. *)
    method write_words_trust_origin_sigs : 
      int -> int -> page_sig_t ->
      string array -> 
      float array -> 
      int array -> 
      string array ->
      Author_sig.packed_author_signature_t array -> page_sig_t

  (** [read_words_trust_origin_sigs page_id rev_id page_sigs] reads
      the words, trust, origin, and author sigs for the revision
      [rev_id] of page [page_id], given the page sigs [page_sigs]. *)
    method read_words_trust_origin_sigs : 
      int -> int -> page_sig_t -> 
      (string array * float array * int array * string array * 
	Author_sig.packed_author_signature_t array)

    (** [delete_author_sigs page_id rev_id] removes from the db the author 
	signatures for [rev_id] of [page_id]. *)
    method delete_author_sigs : int -> int -> page_sig_t -> page_sig_t

    (* Methods on standard revisions *)

    (** [get_rev_text page_id rev_id text_id] returns the text associated with
	text id [text_id] for revision [rev_id] *)
    method read_rev_text : int -> int -> int -> string


    (* ================================================================ *)
    (* User methods. *)

  (** [inc_rep uid delta uname] increments the reputation of user [uid] by
      [delta] in a single operation, so to avoid database problems. 
      [uname] is the username of the user, to ensure that we know 
      the names of the users. *)
    method inc_rep : int -> float -> string -> unit

  (** [set_rep uid rep uname] sets the fact that the reputation for [uid]
      is [rep], and the username is [uname]. *)
    method set_rep : int -> float -> string -> unit

    (** [get_rep uid] gets the reputation of user [uid], from a table 
	relating user ids to their reputation *)
    method get_rep : int -> float

    (** [get_user_id name] gets the user id for the user with the given user name *)
    method get_user_id : string -> int

    (** [write_user_id user_name] writes that the user 
	that has name [user_name]. Returns this new user id *) 
    method write_user_id : string -> int

    (* ================================================================ *)
    (* Voting. *)

    (** Add the vote to the db *)
    method vote : vote_t -> unit

    (* ================================================================ *)
    (* Server System. *)

    (** [mark_page_to_process page_id page_title] specifies that a page
	must be brought up to date, due to a vote or a new revision. *)
    method mark_page_to_process : int -> string -> unit
      
    (** [mark_page_as_processed page_id] 
	marks that a page has been processed. *)
    method mark_page_as_processed : int -> unit

    (** [mark_page_as_unprocessed page_id] marks that a page has not
	been fully processed. *)
    method mark_page_as_unprocessed : int -> unit

  (** [fetch_work_from_queue max_to_get n_retries] gets the
      list of page ids and titles that have to be brought up to date. 
      It also marks those pages as "processing", so that
      subsequent requests do not return the same revisions.  This code
      contains a transaction start / commit pair.  [max_to_get] is the
      maximum number of results to get; [n_retries] is the number of
      times the start / commit pair is used. *)
    method fetch_work_from_queue : int -> int -> int -> (int * string) list

    (** [erase_cached_rev_text page_id force] erases the cached text of all
	revisions of [page_id].  Unless [force] is true, keep_text_cache
	may turn this into a no-op. *)
    method erase_cached_rev_text : int -> bool -> unit

    (* ================================================================ *)
    (* WikiMedia Api *)
    (** [write_page page_to_add] adds adds the given data to the page
	table of the database.  It can be used to import data from the
	mediawiki api and store it locally.  *)
    method write_page : wiki_page_t -> unit

    (** [write_revision revision_to_add] adds the given data to the
	revision and text tables of the database.  It can be used to
	import data from the mediawiki api and store it locally.  *)
    method write_revision : wiki_revision_t -> unit

    (* ================================================================ *)
    (* Debugging. *)
      
    (** Totally clear out the db structure -- THIS IS INTENDED ONLY FOR UNIT
	TESTING *)
    method delete_all : bool -> unit

    (* removed old items from the q. *)
    method init_queue : bool -> unit

  (* ================================================================ *)
  (* Inter-Wiki Coordination. *)
      
    (** [aquire_reservations wikiname desired_allowance max_proc]
	Returns the number of availible processors for the given wiki,
	which is between 0 and desired_allowance.
	Also reserves this for wikiname.
     *)
    method aquire_reservations : string -> int -> int -> int
      
    (** [release_reservation wikiname]
	Marks that wikiname is done with one processing unit.
     *)
    method release_reservation : string -> unit

  end

class db_mediawiki_api : 
  string ->           (* db prefix *)
  Mysql.dbd ->        (* mediawiki db handle *)
  Mysql.dbd option -> (* global (interwiki) db handle *)
  string ->           (* database name *)
  string option ->    (* revision base path *)
  string option ->    (* colored revisions base path *)
  bool ->             (* debug_mode *)
  bool ->             (* keep text cache *)
  object
      inherit db
  end

(** [create_db use_exec_api db_prefix mediawiki_dbh db_name
    rev_base_path sig_base_path colored_base_path debug_mode] returns
    a db of the appropriate type, according to whether we are using
    the exec api or not.  [db_prefix] is the prefix of the db tables;
    [mediawiki_dbh] is the db handle, [db_name] is the name of the
    database (used to ensure lock uniqueness), the base paths point to
    the location of filesystem storage of revision information, and
    [debug_mode] is a flag to facilitate debugging. *)
val create_db :
  bool ->             (* use_exec_api *)
  string ->           (* db prefix *)
  Mysql.dbd ->        (* mediawiki db handle *)
  Mysql.dbd option -> (* global db handle *)
  string ->           (* database name *)
  string option ->    (* revision base path *)
  string option ->    (* colored revisions base path *)
  bool ->             (* debug_mode *)
  bool ->             (* keep text cache *)
  db
