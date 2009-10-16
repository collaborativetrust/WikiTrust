(*

Copyright (c) 2009 The Regents of the University of California
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

open Online_command_line
open Online_types

exception Bad_Line of string

(* Every batch corresponds to 50 revisions, so this will do 1000 at most. *)
let max_batches_to_do = 20
let max_concurrent_procs = 10
let sleep_time_sec = 1
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: downloadwp [options]";;


(* Prepares the database connection information *)
let mediawiki_db = {
  Mysql.dbhost = Some !mw_db_host;
  Mysql.dbname = Some !mw_db_name;
  Mysql.dbport = Some !mw_db_port;
  Mysql.dbpwd  = Some !mw_db_pass;
  Mysql.dbuser = Some !mw_db_user;
}

(* Sets up the db *)
(* Note that here it does not make sense to use the wikipedia API *)
let mediawiki_dbh = Mysql.connect mediawiki_db in
let db = Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh !mw_db_name
  !wt_db_rev_base_path !wt_db_blob_base_path !dump_db_calls in
 
let tabsplit = Str.split_delim (Str.regexp "\t") in

let splitLine2TitleRev line =
    let vals = tabsplit line in
    match vals with
	| [title; rev] -> (title, (int_of_string rev))
	| [title] -> (title, 0)
	| _ -> raise (Bad_Line line)
    in

let main_loop () =
  try
    while true do begin
      let line = input_line stdin in
      let (title, start_rev) = splitLine2TitleRev line in
      try
	Wikipedia_api.download_page_starting_with db title start_rev 0
      with
	Wikipedia_api.API_error msg ->
	  (!Online_log.online_logger)#log (Printf.sprintf "ERROR: %s\nmsg=%s\n" title msg);
    end done
  with End_of_file -> ()
in

main_loop ()
