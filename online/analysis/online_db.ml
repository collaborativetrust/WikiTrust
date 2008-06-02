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

let debug_mode = false;;

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
    
    (* HERE are all of the prepaired sql statments used below *)
    val sth_select_edit_list = "SELECT edit_type, val1,
          val2, val3, version FROM edit_lists WHERE 
          from_revision = ? AND to_revision  = ?"
    val sth_delete_edit_list = "DELETE FROM edit_lists WHERE
          from_revision = ? AND to_revision = ? AND edit_type = ?"      
    val sth_ins_ins_edit_lists = "INSERT INTO edit_lists (from_revision,
          to_revision, edit_type, version, val1, val2 ) VALUES (?, ?, 'Ins', ?, ?, ?) " 
    val sth_ins_mov_edit_lists = "INSERT INTO edit_lists (from_revision,
          to_revision, edit_type, version, val1, val2, val3 ) VALUES (?, ?, 'Mov', ?, ?,
          ?, ?) " 
    val sth_ins_del_edit_lists = "INSERT INTO edit_lists (from_revision,
          to_revision, edit_type, version, val1, val2 ) VALUES (?, ?, 'Del', ?, ?, ?) " 
    val sth_select_user_rep = "SELECT user_rep FROM trust_user_rep 
          WHERE user_id = ?" 
    val sth_update_user_rep = "UPDATE trust_user_rep SET user_rep = 
          ? WHERE user_id = ?" 
    val sth_insert_user_rep = "INSERT INTO trust_user_rep
          (user_id,  user_rep) VALUES (?, ?)" 
    val sth_insert_hist = "INSERT INTO user_rep_history 
          (user_id, rep_before, rep_after, change_time, event_id) VALUES
          (?, ?, ?, ?, NULL)" 
    val sth_delete_markup = "DELETE FROM colored_markup WHERE 
          revision_id = ?" 
    val sth_insert_markup = "INSERT INTO colored_markup 
          (revision_id, revision_text) VALUES (?, ?) " 
    val sth_select_markup = "SELECT revision_text FROM 
          colored_markup WHERE revision_id = ?" 
    val sth_select_dead_chunks_flat = "SELECT timestamp, n_del_revs, ser_text, 
      ser_trust, ser_origin FROM dead_page_chunk_flat WHERE page_id = ?"
    val sth_delete_chunks_flat = "DELETE FROM dead_page_chunk_flat WHERE page_id = ?"
    val sth_insert_chunks_flat = "INSERT INTO dead_page_chunk_flat (page_id,
       timestamp, n_del_revs, ser_trust, ser_origin, ser_text)
       VALUES (?, ?, ?, ?, ?, ?)"
    val sth_delete_feedback = "DELETE FROM feedback WHERE revid1 
          = ? AND revid2 = ?"
    val sth_insert_feedback = "INSERT INTO feedback (revid1, 
          userid1, revid2, userid2, timestamp, q, voided) 
          VALUES (?, ?, ?, ?, ?, ?, ?)"  
    val sth_select_feedback = "SELECT revid2, userid2, timestamp, 
          q, voided FROM feedback WHERE revid1 = ?"   
    val sth_delete_quality = "DELETE FROM quality_info WHERE
          rev_id = ?"
    val sth_insert_quality = "INSERT INTO quality_info
          (rev_id, n_edit_judges, total_edit_quality, min_edit_quality, nix_bit
          ) VALUES (?, ?, ?, ?, ?)" 
    val sth_select_quality = "SELECT rev_id, n_edit_judges,
          total_edit_quality, min_edit_quality, nix_bit
          FROM quality_info WHERE rev_id = ?" 
    val sth_set_colored = "UPDATE revision SET 
          trust_rev_colored = TRUE WHERE rev_id = ?"
   val sth_revert_colored = "UPDATE revision set
          trust_rev_colored = FALSE WHERE rev_id = ?"
    val sth_select_revs = "SELECT rev_id, rev_page, rev_text_id, 
          rev_timestamp, rev_user, rev_user_text, rev_minor_edit, rev_comment FROM            
          revision WHERE rev_page = ? AND rev_id <= ? ORDER BY rev_timestamp DESC"
    val sth_select_all_revs = "SELECT rev_id, rev_page, rev_text_id, 
          rev_timestamp, rev_user, rev_user_text, rev_minor_edit, rev_comment FROM            
          revision ORDER BY rev_timestamp ASC"
    val sth_select_text = "SELECT old_text FROM text WHERE old_id = ?"
    val sth_select_last_colored_rev = "SELECT A.revision_id, B.rev_page FROM 
          colored_markup AS A JOIN revision AS B ON (A.revision_id = B.rev_id) 
          ORDER BY coloredon DESC LIMIT 1"

    (* Returns the last colored rev, if any *)
    method fetch_last_colored_rev : (int * int) =
      match fetch (Mysql.exec dbh sth_select_last_colored_rev) with
        | None -> raise DB_Not_Found
        | Some row -> (not_null int2ml row.(0), not_null int2ml row.(1))

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
      
      let result = Mysql.exec dbh (format_string sth_select_edit_list 
          [Mysql.ml2int revid1; Mysql.ml2int revid2]) in
      
      let handle_head row =
        match not_null str2ml row.(0) with
          | "Ins" -> Editlist.Ins (not_null int2ml row.(1), not_null int2ml row.(2))
          | "Del" -> Editlist.Del (not_null int2ml row.(1), not_null int2ml row.(2))
          | "Mov" -> Editlist.Mov (not_null int2ml row.(1), not_null int2ml row.(2),
              not_null int2ml row.(3))
          | _     -> raise DB_Not_Found    
      in        
      let rec handle_tail res =
        match res with
          | None -> []
          | Some row -> handle_head row :: handle_tail (Mysql.fetch result) 
      in    
      match Mysql.fetch result with 
          | None -> None
          | Some row -> Some (not_null str2ml row.(4), handle_head row :: 
              handle_tail (Mysql.fetch result))
      
     
    (** [wrte_edit_diff revid1 revid2 elist] writes to the database the edit list 
	[elist] from the (live) text of revision [revid1] to revision [revid2]. *)
    method write_edit_diff (revid1 : int) (revid2 : int) (vers : string) (elist : Editlist.edit
    list) : unit = 
      let f ed =
        match ed with
          | Editlist.Ins (i, l) -> (
            ignore (Mysql.exec dbh (format_string sth_delete_edit_list [ml2int revid1; ml2int revid2;
              ml2str "Ins"]));
            ignore (Mysql.exec dbh (format_string sth_ins_ins_edit_lists [ml2int revid1; 
                ml2int revid2; ml2str vers; ml2int i; ml2int l ])))

          | Editlist.Del (i, l) -> (
            ignore (Mysql.exec dbh (format_string sth_delete_edit_list 
                [ml2int revid1; ml2int revid2; ml2str "Del"]));
            ignore (Mysql.exec dbh (format_string sth_ins_del_edit_lists 
                [ml2int revid1; ml2int revid2; ml2str vers; ml2int i; ml2int l ];)))

          | Editlist.Mov (i, j, l) -> (
            ignore (Mysql.exec dbh (format_string sth_delete_edit_list 
                [ml2int revid1; ml2int revid2; ml2str "Mov"])); 
            ignore (Mysql.exec dbh (format_string sth_ins_mov_edit_lists 
                [ml2int revid1; ml2int revid2; ml2str vers; ml2int i; ml2int j; ml2int l ]; )))
          in
      List.iter f elist
    
   
    (** [get_rev_text text_id] returns the text associated with text id [text_id] *)
    method read_rev_text (text_id: int) : string = 
      let result = Mysql.exec dbh (format_string sth_select_text [ml2int text_id]) in 
      match Mysql.fetch result with 
	None -> raise DB_Not_Found
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
      ignore (Mysql.exec dbh "COMMIT")


    (** [set_rep_hist uid t r0 r1] writes, in a table with keys user_id, time, 
	and reputation, that at time [t] the reputation of user [uid] went from
	[r0] to [r1]. *)
    method set_rep_hist (uid : int) (timet : float) (r0 : float) (r1 : float)
    : unit =
      ignore (Mysql.exec dbh (format_string sth_insert_hist 
          [ml2int uid; ml2float r0; ml2float r1; ml2float timet ]));
      ignore (Mysql.exec dbh "COMMIT")

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
      (* First we delete any pre-existing text. *)
      ignore (Mysql.exec dbh (format_string sth_delete_markup 
          [ml2int rev_id ]));
      (* Next we add in the new text. *)
      ignore (Mysql.exec dbh (format_string sth_insert_markup 
          [ml2int rev_id; ml2str markup ]));
      ignore (Mysql.exec dbh "COMMIT")


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
    method write_dead_page_chunks (page_id : int) (clist : Online_types.chunk_t
    list) : unit =
      let f (chk : Online_types.chunk_t) = ( 
            ignore (Mysql.exec dbh (format_string sth_delete_chunks_flat
                [ml2int page_id]));
            ignore (Mysql.exec dbh (format_string sth_insert_chunks_flat
            [ml2int page_id; 
            ml2float chk.timestamp; 
            ml2int chk.n_del_revisions;
            ml2str (string_of__of__sexp_of (sexp_of_array sexp_of_float) chk.trust);
            ml2str (string_of__of__sexp_of (sexp_of_array sexp_of_int) chk.origin);
            ml2str (string_of__of__sexp_of (sexp_of_array sexp_of_string) chk.text)
            ]));
      ) in
      List.iter f clist;
      ignore (Mysql.exec dbh "COMMIT")

    (** [read_dead_page_chunks page_id] returns the list of dead chunks associated
	with the page [page_id]. *)
    method read_dead_page_chunks (page_id : int) : Online_types.chunk_t list =
      let handle_row row = (
        { timestamp = not_null float2ml row.(0);
          n_del_revisions = not_null int2ml row.(1);
          text = (of_string__of__of_sexp (array_of_sexp string_of_sexp)
              (not_null str2ml row.(2)));
          trust = (of_string__of__of_sexp (array_of_sexp float_of_sexp)
              (not_null str2ml row.(3)));
          origin = (of_string__of__of_sexp (array_of_sexp int_of_sexp) 
              (not_null str2ml row.(4)));
        }
      ) in
      let result = Mysql.exec dbh (format_string sth_select_dead_chunks_flat
          [ml2int page_id ]) in
      let rec loop = function
        | None      -> []
        | Some x    -> handle_row x :: loop (Mysql.fetch result)
      in
      loop (Mysql.fetch result)
      
      

  (** [write_quality_info rev_id n_edit_judges total_edit_quality min_edit_quality
   n_text_judges new_text persistent_text] writes in a table on disk
   indexed by [rev_id] the tuple (rev_id  n_edit_judges total_edit_quality
    min_edit_quality n_text_judges new_text persistent_text). *)

    method write_quality_info (rev_id : int) (q: qual_info_t) : unit = 
      (* First we delete any pre-existing text. *)
      ignore (Mysql.exec dbh (format_string sth_delete_quality [ml2int rev_id ]));
      (* Next we add in the new text. *)
      ignore (Mysql.exec dbh (format_string sth_insert_quality 
          [ml2int rev_id; ml2int q.n_edit_judges;
          ml2float q.total_edit_quality; ml2float q.min_edit_quality; if q.nix_bit then "1" else "0"]));
      ignore (Mysql.exec dbh "COMMIT")    


    (** [read_quality_info rev_id] returns the tuple 
       (n_edit_judges total_edit_quality min_edit_quality
             n_text_judges new_text persistent_text)
          associated with the revision with id [rev_id].
    *)

    method read_quality_info (rev_id : int) : qual_info_t option = 
      let result = Mysql.exec dbh (format_string sth_select_quality [ml2int rev_id]) in
      match fetch result with
        | None -> None
        | Some x -> Some {n_edit_judges = not_null int2ml x.(1); 
                          total_edit_quality = not_null float2ml x.(2); 
                          min_edit_quality = not_null float2ml x.(3);
                          nix_bit = (not_null int2ml x.(4) > 0)}


    (** [mark_rev_colored rev_id] marks a revision as having been colored *)
    (*
    method mark_rev_colored (rev_id : int) : unit =
      ignore (Mysql.exec dbh (format_string sth_set_colored [ml2int rev_id]))*)
    (** [mark_rev_uncolored revid] removed the marking of a revision
       as having been colored *)(*
    method mark_rev_uncolored (rev_id : int) : unit =
      ignore (Mysql.exec dbh (format_string sth_revert_colored [ml2int rev_id]))
*)
  (*  method prepare_cached (sql : string) : Dbi.statement =
      dbh#prepare_cached sql*)


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
            ignore (Mysql.exec dbh "TRUNCATE TABLE text_split_version" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE edit_lists" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE trust_user_rep" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE user_rep_history" ); 
            ignore (Mysql.exec dbh "TRUNCATE TABLE colored_markup" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE dead_page_chunks" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE chunk_text" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE chunk_trust" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE chunk_origin" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE dead_page_chunk_map" );
            ignore (Mysql.exec dbh "TRUNCATE TABLE quality_info" ); 
            ignore (Mysql.exec dbh "TRUNCATE TABLE dead_page_chunk_flat" ); 
            ignore (Mysql.exec dbh "COMMIT"))
        | false -> ignore (Mysql.exec dbh "COMMIT")

  end;; (* online_db *)

