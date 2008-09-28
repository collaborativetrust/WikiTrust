(*

Copyright (c) 2008 The Regents of the University of California
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

open Printf
open Mysql

(** MissingInformation is raised if any of 
    page_id, revision_id, or voter_uid is not specified. *)
exception MissingInformation

(** This is the top-level code for voting on revisions. *)

(* Mediawiki DB *)
let mw_db_user = ref "wikiuser"
let set_mw_db_user u = mw_db_user := u
let mw_db_pass = ref ""
let set_mw_db_pass p = mw_db_pass := p
let mw_db_name = ref "wikidb"
let set_mw_db_name d = mw_db_name := d
let mw_db_host = ref "localhost"
let set_mw_db_host d = mw_db_host := d
let mw_db_port = ref 3306
let set_mw_db_port d = mw_db_port := d

(* Wikitrust DB *)
let use_separate_dbs = ref false
let wt_db_user = ref "wikiuser"
let set_wt_db_user u = wt_db_user := u; use_separate_dbs := true
let wt_db_pass = ref ""
let set_wt_db_pass p = wt_db_pass := p; use_separate_dbs := true
let wt_db_name = ref "wikidb"
let set_wt_db_name d = wt_db_name := d; use_separate_dbs := true
let wt_db_host = ref "localhost"
let set_wt_db_host d = wt_db_host := d; use_separate_dbs := true
let wt_db_port = ref 3306
let set_wt_db_port d = wt_db_port := d; use_separate_dbs := true

(* Other paramiters *)
let db_prefix = ref ""
let set_db_prefix d = db_prefix := d
let log_name = ref "/dev/null"
let set_log_name d = log_name := d
let synch_log = ref false
let noop s = ()
let rev_id_opt = ref None
let set_rev_id d = rev_id_opt := Some d
let page_id_opt = ref None
let set_page_id d = page_id_opt := Some d
let voter_id_opt = ref None
let set_voter_id d = voter_id_opt := Some d
let vote_time_opt = ref None
let set_vote_time d = vote_time_opt := Some d
let times_to_retry_trans = ref 3
let set_times_to_retry_trans n = times_to_retry_trans := n
let dump_db_calls = ref false

(* Figure out what to do and how we are going to do it. *)
let command_line_format = 
  [
    ("-db_prefix", Arg.String set_db_prefix, "<string>: Database table prefix (default: none)");
   ("-db_user", Arg.String set_mw_db_user, "<string>: Mediawiki DB username (default: wikiuser)");
   ("-db_name", Arg.String set_mw_db_name, "<string>: Mediawiki DB name (default: wikidb)");
   ("-db_pass", Arg.String set_mw_db_pass, "<string>: Mediawiki DB password");
   ("-db_host", Arg.String set_mw_db_host, "<string>: Mediawiki DB host (default: localhost)");
   ("-db_port", Arg.Int set_mw_db_port,    "<int>: Mediawiki DB port (default: 3306)");

   ("-wt_db_user", Arg.String set_wt_db_user, "<string>: Wikitrust DB username (specify only if the wikitrust db is different from the mediawiki db) (default: wikiuser)");
   ("-wt_db_name", Arg.String set_wt_db_name, "<string>: Wikitrust DB name (specify only if the wikitrust db is different from the mediawiki db) (default: wikidb)");
   ("-wt_db_pass", Arg.String set_wt_db_pass, "<string>: Wikitrust DB password (specify only if the wikitrust db is different from the mediawiki db)");
   ("-wt_db_host", Arg.String set_wt_db_host, "<string>: Wikitrust DB host (specify only if the wikitrust db is different from the mediawiki db) (default: localhost)");
   ("-wt_db_port", Arg.Int set_wt_db_port, "<int>: Wikitrust DB port (specify only if the wikitrust db is different from the mediawiki db) (default: 3306)");

   ("-rev_id",  Arg.Int set_rev_id, "<int>: revision ID that is voted");
   ("-page_id", Arg.Int set_page_id, "<int>: page ID that is voted");
   ("-voter_id", Arg.Int set_voter_id, "<int>: user ID that votes");
   ("-vote_time", Arg.String set_vote_time, "<string>: timestamp for the vote in form YYYYMMDDHHMMSS. Ex: 20080927231134");

   ("-log_file", Arg.String set_log_name, "<filename>: Logger output file (default: /dev/null)");
   ("-times_to_retry_trans", Arg.Int set_times_to_retry_trans, "<int>: Max number of times to retry a transation if it fails (default: 3)."); 
   ("-dump_db_calls", Arg.Set dump_db_calls, ": Writes to the db log all database calls.  This is very verbose; use only for debugging.");
  ]

let _ = Arg.parse command_line_format noop "
This command lets users vote for the quality of revisions. 
In the call
  vote_revision ... -rev_id 4 -page_id 5 -voter_id 6 ...
user 6 votes for the quality of the revision with id 4, belonging to the page
with id 5.  The revision with id 4 needs to be the most recent of the page 
with id 5, otherwise nothing is done. 

Usage: vote_revision";;

let trust_coeff = Online_types.get_default_coeff;;

(* Prepares the data for the db connections. *)
let mediawiki_db = {
  dbhost = Some !mw_db_host;
  dbname = Some !mw_db_name;
  dbport = Some !mw_db_port;
  dbpwd  = Some !mw_db_pass;
  dbuser = Some !mw_db_user;
}
let wikitrust_db_opt = 
  if !use_separate_dbs 
  then Some { 
    dbhost = Some !wt_db_host;
    dbname = Some !wt_db_name;
    dbport = Some !wt_db_port;
    dbpwd  = Some !wt_db_pass;
    dbuser = Some !wt_db_user;
  }
  else None

(* Extracts page, revision, and user id, complaining if they are not there. *)
let page_id = match !page_id_opt with 
    None -> raise MissingInformation
  | Some d -> d;;
let revision_id = match !rev_id_opt with 
    None -> raise MissingInformation
  | Some d -> d;;
let voter_id = match !voter_id_opt with 
    None -> raise MissingInformation
  | Some d -> d;;
let vote_time = match !vote_time_opt with 
    None -> raise MissingInformation
  | Some d -> d;;

(* Opens the log file *)
let logger = new Online_log.logger !log_name !synch_log;;
(* Opens the db connections. *)
let db = new Online_db.db !db_prefix mediawiki_db wikitrust_db_opt !dump_db_calls;;

(* Add the vote to the db *)
db#vote {
  Online_db.vote_time = vote_time;
  Online_db.vote_page_id = page_id;
  Online_db.vote_revision_id = revision_id;
  Online_db.vote_voter_id = voter_id;
};;


