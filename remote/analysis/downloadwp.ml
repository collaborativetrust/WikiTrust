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

(* evry batch corresponds to 50 revisions, so this will do 1000 at most. *)
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
let db = new Online_db.db !db_prefix mediawiki_db None 
  !wt_db_rev_base_path !wt_db_sig_base_path !wt_db_colored_base_path 
  !dump_db_calls in
let logger = new Online_log.logger !log_name !synch_log in


let rec download_page title last_rev =
  let next_rev = Wikipedia_api.get_revs_from_api title last_rev db logger 50 in 
  let _ = Unix.sleep sleep_time_sec in
  match next_rev with
      Some next_id -> begin
	logger#log (Printf.sprintf "Loading next batch: %s -> %d\n" title next_id);
	download_page title next_id
      end
    | None -> ()
in

let main_loop () =
  try
    while true do
      let title = input_line stdin in
      let lastid =
	try
	  db#get_latest_rev_id title
	with Online_db.DB_Not_Found -> 0
      in
	download_page title lastid;
    done
  with End_of_file -> ()
in

main_loop ()
