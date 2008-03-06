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
    implementation.
    I don't know how it is created; most likely, it would take a database 
    name and password, or something like that, to connect to the db. *)
class db  
  (user : string)
  (auth : string)
  (database : string) =

  object(self)
        
    val mutable dbh = Dbi_mysql.connect
                        ~user:user
                        ~password:auth
                        database 
    (** [read_text_split_version page_id] given a [page_id] returns a 
	string associated with the page in the db.  The string 
	represents the version of Text.ml that has 
	been used to split a revision in words. *)
    method read_text_split_version (page_id : int) : string = 
      let sth = dbh#prepare "SELECT page_text FROM text_split_version WHERE 
                                page_id = ?" in
      sth#execute [`Int page_id];
      sth#fetch1string () 

    (** [write_text_split_version page_id s] writes to the db 
	a string [s] associated with a page [page_id].  The string
	represents the version of Text.ml that has 
	been used to split a revision in words, and returns it. *)
    method write_text_split_version (page_id : int) (page_text : string) : unit =
      (* First we delete any pre-existing text. *)
      let sth = dbh#prepare "DELETE FROM text_split_version WHERE 
                             page_id = ?" in
      sth#execute [`Int page_id ];
        
      (* Next we add in the new text. *)
      let sth = dbh#prepare "insert into text_split_version (page_text, page_id)
                               values (?, ?)" in
      sth#execute [`String page_text; `Int page_id ];
      dbh#commit ()


      
    (** [read_edit_diff revid1 revid2] reads from the database the edit list 
	from the (live) text of revision [revid1] to revision [revid2]. *)
    method read_edit_diff (revid1 : int) (revid2 : int) : (Editlist.edit list) =
      let sth = dbh#prepare "SELECT edit_type, val1,
          val2, val3 FROM edit_lists WHERE 
          from_revision = ? AND to_revision  = ?" in
      sth#execute [`Int revid1; `Int revid2];
      let elist = [] in
      sth#iter (function
                | [`String etype; `Int val1; `Int val2; `Int val3] ->
                  match etype with
                    "Ins" -> Ins (val1, val2) :: elist
                    | "Del" -> Del (val1, val2) :: elist
                    | "Mov" -> Mov (val1, val2, val3) :: elist
                | _ ->
                    assert false );
      elist              
      

      
    (** [write_edit_diff revid1 revid2 elist] writes to the database the edit list 
	[elist] from the (live) text of revision [revid1] to revision [revid2]. *)
    method write_edit_diff (revid1 : int) (revid2 : int) (elist : Editlist.edit
    list) : unit = 
      let sth_ins = dbh#prepare "insert into edit_lists (from_revision,
           to_revisiona, edit_type, val1, val2 ) VALUES (?, ?, 'Ins', ?, ?) " in
      let sth_mov = dbh#prepare "insert into edit_lists (from_revision,
           to_revisiona, edit_type, val1, val2, val3 ) VALUES (?, ?, 'Mov', ?,
           ?, ?) " in
      let sth_del = dbh#prepare "insert into edit_lists (from_revision,
           to_revisiona, edit_type, val1, val2 ) VALUES (?, ?, 'Del', ?, ?) " in

      let f rfr rto ed =
        match ed with
            Ins (i, l) -> sth_ins#execute [`Int rfr; `Int rto; `Int i; `Int l ];

          | Del (i, l) -> sth_del#execute [`Int rfr; `Int rto; `Int i; `Int l ];

          | Mov (i, j, l) -> sth_mov#execute [`Int rfr; `Int rto; `Int i;
                  `Int j; `Int l ]; 
          in
      List.iter f revid1 revid2 elist
    

    (** [get_rep uid] gets the reputation of user [uid], from a table 
	relating user ids to their reputation *)
    method get_rep (uid : int) : float =
      let sth = dbh#prepare "SELECT user_rep FROM trust_user_rep WHERE 
                                      user_id = ?" in
      sth#execute [`Int uid];
      float_of_string (sth#fetch1string ())


    (** [set_rep uid r] sets, in the table relating user ids to reputations, 
	the reputation of user [uid] to be equal to [r]. *)
    method set_rep (uid : int) (rep : float) =
      (* first check to see if there exists the user already *)
      
      let old_rep = self#get_rep uid in
      match old_rep with
        _  -> 
          let sth = dbh#prepare "insert into trust_user_rep (user_id,
                                 user_rep) values (?, ?)" in
          sth#execute [`Int uid; `Float rep ];
          dbh#commit
        | float ->  
          let sth = dbh#prepare "update trust_user_rep set user_rep = ?
                                 where user_id = ?" in
          sth#execute [`Int uid; `Float rep ];
          dbh#commit ()

    (** [set_rep_hist uid t r0 r1] writes, in a table with keys user_id, time, 
	and reputation, that at time [t] the reputation of user [uid] went from
	[r0] to [r1]. *)
    method set_rep_hist (uid : int) (timet : float) (r0 : float) (r1 : float)
    : unit =
      (* First we delete any pre-existing text. *)
      let sth = dbh#prepare "DELETE FROM user_rep_history WHERE user_id = ? AND
          change_time = ?" in
      sth#execute [`Int uid; `Float timet ];

      (* Next we add in the new text. *)
      let sth = dbh#prepare "insert into user_rep_history (user_id, rep_before,
          rep_after, change_time) VALUES
          (?, ?, ?, ?)" in
      sth#execute [`Int uid; `Float r0; `Float r1; `Float timet ];
      dbh#commit ()


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
      let sth = dbh#prepare "DELETE FROM colored_markup WHERE revision_id =
          ?" in
      sth#execute [`Int rev_id ];
      (* Next we add in the new text. *)
      let sth = dbh#prepare "insert into colored_markup (revision_id,
          revision_text) VALUES (?, ?) " in
      sth#execute [`Int rev_id; `String markup ];
      dbh#commit ()

    (** [read_colored_markup rev_id] reads the text markup of a revision with id
	[rev_id].  The markup is the text of the revision, annontated with trust
	and origin information. *)
    method read_colored_markup (rev_id : int) : string =
      let sth = dbh#prepare "SELECT revision_text FROM colored_markup WHERE 
          revision_id = ?" in
      sth#execute [`Int rev_id];
      sth#fetch1string ()

(*
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
*)
  end;; (* online_db *)

