(*

Copyright (c) 2009 The Regents of the University of California
All rights reserved.

Authors: Ian Pye

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

(*
  read_revision -- given a revision id and database params, outputs the 
  colored markup for this revision to stdout.
*)

open Online_command_line;;

(* Figure out what to do and how we are going to do it. *)
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: read_revision -rev_id -db_user ...\n";;

let main (rev_id : int) (db : Online_db.db) : unit =
  let (rev_info, _, blob_id) = db#read_wikitrust_revision rev_id in
  Printf.printf "%s\n"
    (db#read_colored_markup rev_info.Online_db.rev_id rev_id blob_id)
in

(* Prepares the database connection information *)
let mediawiki_db = {
  Mysql.dbhost = Some !mw_db_host;
  Mysql.dbname = Some !mw_db_name;
  Mysql.dbport = Some !mw_db_port;
  Mysql.dbpwd  = Some !mw_db_pass;
  Mysql.dbuser = Some !mw_db_user;
} in
let mediawiki_dbh = Mysql.connect mediawiki_db in 
let db = Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh None
  !mw_db_name !wt_db_rev_base_path !wt_db_blob_base_path !dump_db_calls 
in 
match !requested_rev_id with
| None -> raise (Invalid_argument "-rev_id paramiter required")
| Some r -> main r db
