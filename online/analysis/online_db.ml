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

open Online_types
open Mysql
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"

type foo_t = int list with sexp;;

let debug_mode = false;;

(* Should a commit be issued after every insert? This is needed if there are multiple clients. *)
let commit_frequently = true;;

let identity x = x;;

let rec format_string (str : string) (vals : string list) : string =                    
  match vals with                                                                       
    | [] -> if debug_mode then print_endline str; str
    | hd::tl -> (match (ExtString.String.replace str "?" hd) with                       
      | (true, newstr) -> format_string newstr tl                                       
      | (false, newstr) -> newstr)                                                      
;;

exception DB_Not_Found

(** This class provides a handle for accessing the database in the on-line 
    implementation. *)

class db  
  (user : string)
  (auth : string)
  (database : string) =
 
  let db_param = {dbhost = None;
                  dbport = None;
                  dbname = Some database; 
                  dbpwd = Some auth;
                  dbuser = Some user} in
  let dbh = (Mysql.connect db_param) in
   
  object(self)
     
    (* Stats values *) 
    val rep_epsilon = 0.1  
    val start_time = Unix.gettimeofday ()
    val mutable total_rep_change = 0.
    val mutable num_rep_changes = 0
    val mutable num_changes_over_epsilon= 0
    val mutable num_changes_under_epsilon = 0

    (* HERE are all of the prepaired sql statments used below *)
    val sth_select_edit_list_flat = "SELECT version, edits FROM wikitrust_edit_lists
        WHERE from_revision = ? AND to_revision  = ?"
    val sth_insert_edit_list_flat = "INSERT INTO wikitrust_edit_lists (version, edits, 
        from_revision, to_revision) VALUES (?, ?, ?, ?) ON DUPLICATE KEY UPDATE
        version = ?, edits = ?"
    val sth_select_user_rep = "SELECT user_rep FROM wikitrust_trust_user_rep 
          WHERE user_id = ?" 
    val sth_update_user_rep = "UPDATE wikitrust_trust_user_rep SET user_rep = 
          ? WHERE user_id = ?" 
    val sth_insert_user_rep = "INSERT INTO wikitrust_trust_user_rep
          (user_id,  user_rep) VALUES (?, ?)" 
    val sth_insert_hist = "INSERT INTO wikitrust_user_rep_history 
          (user_id, rep_before, rep_after, change_time, event_id) VALUES
          (?, ?, ?, ?, NULL)" 
    val sth_delete_markup = "DELETE FROM wikitrust_colored_markup WHERE revision_text = ?"      
    val sth_insert_markup = "INSERT INTO wikitrust_colored_markup 
          (revision_id, revision_text) VALUES (?, ?)" 
    val sth_select_markup = "SELECT revision_text FROM wikitrust_colored_markup 
          WHERE revision_id = ?" 
    val sth_select_dead_chunks = "SELECT chunks FROM wikitrust_dead_page_chunks WHERE page_id = ?"
    val sth_delete_chunks = "DELETE FROM wikitrust_dead_page_chunks WHERE page_id = ?"
    val sth_insert_dead_chunks = "INSERT INTO wikitrust_dead_page_chunks (page_id, chunks) 
       VALUES (?, ?)"
    val sth_insert_quality = "INSERT INTO wikitrust_quality_info
          (rev_id, n_edit_judges, total_edit_quality, min_edit_quality, nix_bit
          ) VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE n_edit_judges = ?,
          total_edit_quality = ?, min_edit_quality = ?, 
          nix_bit = ?" 
    val sth_select_quality = "SELECT rev_id, n_edit_judges,
          total_edit_quality, min_edit_quality, nix_bit
          FROM wikitrust_quality_info WHERE rev_id = ?" 
    val sth_select_revs = "SELECT rev_id, rev_page, rev_text_id, 
          rev_timestamp, rev_user, rev_user_text, rev_minor_edit, rev_comment 
          FROM revision WHERE rev_page = ? AND rev_id <= ? ORDER BY rev_timestamp DESC"
    val sth_select_all_revs = "SELECT rev_id, rev_page, rev_text_id, 
          rev_timestamp, rev_user, rev_user_text, rev_minor_edit, rev_comment 
          FROM revision ORDER BY rev_timestamp ASC"
    val sth_select_all_revs_after = "SELECT rev_id, rev_page, rev_text_id, 
          rev_timestamp, rev_user, rev_user_text, rev_minor_edit, rev_comment 
          FROM revision WHERE rev_timestamp > ? ORDER BY rev_timestamp ASC"      
    val sth_select_text = "SELECT old_text FROM text WHERE old_id = ?"
    val sth_select_last_colored_rev = "SELECT A.revision_id, B.rev_page, A.coloredon 
          FROM wikitrust_colored_markup AS A JOIN revision AS B ON (A.revision_id = B.rev_id) 
          ORDER BY coloredon DESC LIMIT 1"

    (* Returns the last colored rev, if any *)
    method fetch_last_colored_rev : (int * int * (int * int * int * int * int * int)) =
      match fetch (Mysql.exec dbh sth_select_last_colored_rev) with
        | None -> raise DB_Not_Found
        | Some row -> (not_null int2ml row.(0), not_null int2ml row.(1), 
            not_null timestamp2ml row.(2))
  
    (** [sth_select_all_revs_after (int * int * int * int * int * int)] returns all 
        revs created after the given timestamp. *)
    method fetch_all_revs_after (timesmp : (int * int * int * int * int * int)) : Mysql.result =   
      Mysql.exec dbh (format_string sth_select_all_revs_after [ml2timestamp timesmp])

    (** [fetch_all_revs] returns a cursor that points to all revisions in the database, 
	in ascending order of timestamp. *)
    method fetch_all_revs : Mysql.result = 
      Mysql.exec dbh (format_string sth_select_all_revs [])
  
    (** [fetch_revs page_id rev_id] returns a cursor that points to all revisions 
	of page [page_id] that coincide, or precede, revision [rev_id]. *)
    method fetch_revs (page_id : int) (rev_id : int) : Mysql.result =
      Mysql.exec dbh (format_string sth_select_revs [ml2int page_id; ml2int rev_id])

    (** [read_edit_diff revid1 revid2] reads from the database the edit list 
	from the (live) text of revision [revid1] to revision [revid2]. *)
    method read_edit_diff (revid1 : int) (revid2 : int) : 
      (string * (Editlist.edit list)) option =
      let result = Mysql.exec dbh (format_string sth_select_edit_list_flat
          [Mysql.ml2int revid1; Mysql.ml2int revid2]) 
      in
      match Mysql.fetch result with 
          | None -> None
          | Some row -> Some (not_null str2ml row.(0),
              of_string__of__of_sexp (list_of_sexp Editlist.edit_of_sexp) 
              (not_null str2ml row.(1)))
      
     
    (** [wrte_edit_diff revid1 revid2 elist] writes to the database the edit list 
	[elist] from the (live) text of revision [revid1] to revision [revid2]. *)
    method write_edit_diff (revid1 : int) (revid2 : int) (vers : string) 
        (elist : Editlist.edit list) : unit = 
      (* Next we add in the new text. *)
      ignore (Mysql.exec dbh (format_string sth_insert_edit_list_flat
          [ml2str vers; 
          ml2str (string_of__of__sexp_of (sexp_of_list Editlist.sexp_of_edit) elist);
          ml2int revid1; 
          ml2int revid2;
          ml2str vers;
          ml2str (string_of__of__sexp_of (sexp_of_list Editlist.sexp_of_edit) elist);
          ]));
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")

    (** [get_rev_text text_id] returns the text associated with text id [text_id] *)
    method read_rev_text (text_id: int) : string = 
      let result = Mysql.exec dbh (format_string sth_select_text [ml2int text_id]) in 
      match Mysql.fetch result with 
        | None -> raise DB_Not_Found
        | Some y -> not_null str2ml y.(0)


    (** [get_rep uid] gets the reputation of user [uid], from a table 
	      relating user ids to their reputation 
        @raise DB_Not_Found if no tuple is returned by the database.
    *)
    method get_rep (uid : int) : float =
      let result = Mysql.exec dbh (format_string sth_select_user_rep [ml2int uid]) in
      match Mysql.fetch result with 
        | None -> raise DB_Not_Found
        | Some x -> not_null float2ml x.(0)
      

    (** [set_rep uid r] sets, in the table relating user ids to reputations, 
	  the reputation of user [uid] to be equal to [r]. *)
    method set_rep (uid : int) (rep : float) =
      (* first check to see if there exists the user already *)
      try
        ignore (self#get_rep uid ) ;
        ignore (Mysql.exec dbh (format_string sth_update_user_rep 
            [ml2float rep; ml2int uid ])); 
      with
        DB_Not_Found -> 
          ignore (Mysql.exec dbh (format_string sth_insert_user_rep 
              [ml2int uid; ml2float rep ]));  
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")
  
    method print_stats : unit =
      print_endline ("Done in " ^ (string_of_float ((Unix.gettimeofday ()) -. start_time)));
      print_endline (string_of_float (total_rep_change /. (float_of_int num_rep_changes))
          ^ " Average change.");
      print_endline ((string_of_int num_changes_over_epsilon ) ^ " Changes over " 
          ^ (string_of_float rep_epsilon));
      print_endline ((string_of_int num_changes_under_epsilon ) ^ " Changes under.");
      print_endline ((string_of_int num_rep_changes) ^ " Total changes.")

    (** [set_rep_hist uid t r0 r1] writes, in a table with keys user_id, time, 
	and reputation, that at time [t] the reputation of user [uid] went from
	[r0] to [r1]. *)
    method set_rep_hist (uid : int) (timet : float) (r0 : float) (r1 : float)
    : unit =
      let delta = abs_float (r1 -. r0) in
      total_rep_change <- total_rep_change +. delta;
      num_rep_changes <- num_rep_changes + 1;
      match (delta > rep_epsilon) with  
        | true -> num_changes_over_epsilon <- num_changes_over_epsilon + 1 
        | false -> num_changes_under_epsilon <- num_changes_under_epsilon + 1;
      ignore (Mysql.exec dbh (format_string sth_insert_hist 
          [ml2int uid; ml2float r0; ml2float r1; ml2float timet ]));
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")

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
    (* This is currently a first cut, which will be hopefully optimized later *)
    method write_colored_markup (rev_id : int) (markup : string) : unit =
      (* Next we add in the new text. *)
      ignore (Mysql.exec dbh (format_string sth_delete_markup
          [ml2int rev_id ]));
      ignore (Mysql.exec dbh (format_string sth_insert_markup 
          [ml2int rev_id; ml2str markup ]));
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")


    (** [read_colored_markup rev_id] reads the text markup of a revision with id
	[rev_id].  The markup is the text of the revision, annontated with trust
	and origin information. *)
    method read_colored_markup (rev_id : int) : string =
      let result = Mysql.exec dbh (format_string sth_select_markup 
          [ml2int rev_id]) in
      match Mysql.fetch result with
        | None -> raise DB_Not_Found
        | Some x -> not_null str2ml x.(0)


    (** [write_dead_page_chunks page_id chunk_list] writes, in a table indexed by 
	(page id, string list) that the page with id [page_id] is associated 
	with the "dead" strings of text [chunk1], [chunk2], ..., where
	[chunk_list = [chunk1, chunk2, ...] ]. 
	The chunk_list contains text that used to be present in the article, but has 
	been deleted; the database records its existence. *)
    method write_dead_page_chunks (page_id : int) (c_list : Online_types.chunk_t list) : unit = 
      let chunks_string = ml2str (string_of__of__sexp_of 
          (sexp_of_list sexp_of_chunk_t) c_list) in 
      ignore (Mysql.exec dbh (format_string sth_delete_chunks
        [ml2int page_id]));
      ignore (Mysql.exec dbh (format_string sth_insert_dead_chunks 
	[ml2int page_id; chunks_string ])); 
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")


    (** [read_dead_page_chunks page_id] returns the list of dead chunks associated
	with the page [page_id]. *)
    method read_dead_page_chunks (page_id : int) : Online_types.chunk_t list =
      let result = Mysql.exec dbh (format_string sth_select_dead_chunks
          [ml2int page_id ]) in 
      match Mysql.fetch result with 
	None -> raise DB_Not_Found
      | Some x -> of_string__of__of_sexp (list_of_sexp chunk_t_of_sexp) 
                      (not_null str2ml x.(0))

  (** [write_quality_info rev_id n_edit_judges total_edit_quality min_edit_quality
   n_text_judges new_text persistent_text] writes in a table on disk
   indexed by [rev_id] the tuple (rev_id  n_edit_judges total_edit_quality
    min_edit_quality n_text_judges new_text persistent_text). *)


    method write_quality_info (rev_id : int) (q: qual_info_t) : unit = 
      (* Next we add in the new text. *)
      ignore (Mysql.exec dbh (format_string sth_insert_quality 
          [ml2int rev_id; ml2int q.n_edit_judges;
          ml2float q.total_edit_quality; 
	  ml2float q.min_edit_quality; 
	  if q.nix_bit then "1" else "0";
          ml2int q.n_edit_judges;
          ml2float q.total_edit_quality; 
	  ml2float q.min_edit_quality; 
	  if q.nix_bit then "1" else "0"
          ]));
      if commit_frequently then ignore (Mysql.exec dbh "COMMIT")    


    (** [read_quality_info rev_id] returns the tuple 
       (n_edit_judges total_edit_quality min_edit_quality
             n_text_judges new_text persistent_text)
          associated with the revision with id [rev_id]. *)
    method read_quality_info (rev_id : int) : qual_info_t option = 
      let result = Mysql.exec dbh (format_string sth_select_quality [ml2int rev_id]) in
      match fetch result with
        | None -> None
        | Some x -> Some {n_edit_judges = not_null int2ml x.(1); 
                          total_edit_quality = not_null float2ml x.(2); 
                          min_edit_quality = not_null float2ml x.(3);
                          nix_bit = (not_null int2ml x.(4) > 0)}


    (** [get_page_lock page_id] gets a lock for page [page_id], to guarantee 
	mutual exclusion on the updates for page [page_id]. *)
    method get_page_lock (page_id: int) = ()

    (** [release_page_lock page_id] releases the lock for page [page_id], to guarantee 
	mutual exclusion on the updates for page [page_id]. *)
    method release_page_lock (page_id: int) = ()

    (** [get_rep_lock] gets a lock for the global table of user reputations, to guarantee 
	serializability of the updates. *)
    method get_rep_lock = ()

    (** [release_rep_lock] releases a lock for the global table of user reputations, to guarantee 
	serializability of the updates. *)
    method release_rep_lock = ()

    (** Clear everything out *)
    method delete_all (really : bool) =
      match really with
        | true -> (
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_edit_lists" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_trust_user_rep" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_user_rep_history" ); 
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_colored_markup" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_dead_page_chunks" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE wikitrust_quality_info" ); 
            ignore (Mysql.exec dbh "COMMIT"))
        | false -> ignore (Mysql.exec dbh "COMMIT")

  end;; (* online_db *)

