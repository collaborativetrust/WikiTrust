(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, B. Thomas Adler, Ian Pye

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

(** This is the top-level code of the wiki online xml evaluation. 
    This is used for testing only: *)
let db_user = ref ""
let set_db_user u = db_user := u
let db_pass = ref ""
let set_db_pass p = db_pass := p
let db_name = ref ""
let set_db_name d = db_name := d
let log_name = ref ""
let set_log_name d = log_name := d
let synch_log = ref false
let noop s = ()

(* Figure out what to do and how we are going to do it. *)
let command_line_format = 
  [("-db_user", Arg.String set_db_user, "<user>: DB user to use");
   ("-db_name", Arg.String set_db_name, "<name>: Name of the DB to use");
   ("-db_pass", Arg.String set_db_pass, "<pass>: DB password");
   ("-log_name", Arg.String set_log_name, "<logger.out>: Logger output file");
   ("-synch_log", Arg.Set synch_log, ": Runs the logger in synchnonized mode");
  ]

let _ = Arg.parse command_line_format noop "Usage: eval_online_wiki";;

(* Does all the work of processing the given page and revision *)
let db = new Online_db.db !db_user !db_pass !db_name in
let logger = new Online_log.logger !log_name !synch_log in
let trust_coeff = Online_types.get_default_coeff in
(* Erases old coloring *)
db#delete_all true;

(* Loops over all revisions, in chronological order *)
let revs = db#fetch_all_revs in 
let domore = ref true in 
while !domore do begin 
  match Mysql.fetch revs with 
    None -> domore := false
  | Some r -> begin 
      let rev = Online_revision.make_revision r db in 
      let page_id = rev#get_page_id in 
      let rev_id  = rev#get_id in 
      Printf.printf "Evaluating revision %d of page %d\n" rev_id page_id;
      let page = new Online_page.page db logger page_id rev_id trust_coeff in
      page#eval
    end
end done;

(* Commit the transaction *)
ignore db#commit;

(* Print some stats on what happened. *)
db#print_stats;;


