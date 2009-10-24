(*

Copyright (c) 2007-2009 The Regents of the University of California
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

open Mysql;;
open Online_command_line;;
open Online_types;;

(** MissingInformation is raised if any of 
    page_id, revision_id, or voter_uid is not specified. *)
exception MissingInformation

(** This is the top-level code of the wiki online xml evaluation. *)

(* Figure out what to do and how we are going to do it. *)
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "
This command computes user reputations and text trust for a wiki. 
The command assumes that the wiki database already contains some special 
tables for reputation and trust, and computes the missing reputation and 
trust values, in chronological order.  The code is thread-safe, meaning
that more than one instance can be active at the same time; an instance
terminates when all the work is done, or if there are too many active
instances (measured not from the number of active instances, but from 
the amount of DB contention that is generated, so the code is 
auto-throttling).  The command can be called whenever someone edits a 
revision, in which case it will just color the latest revision 
according to trust, and it will update user reputations accordingly.

Usage: eval_online_wiki";;

let n_processed_events = ref 0;;
let trust_coeff = Online_types.get_default_coeff;;
let f m n = !reputation_speed *. (Online_types.default_dynamic_rep_scaling n m) in 
trust_coeff.Online_types.dynamic_rep_scaling <- f;;

(* There are two types of throttle delay: a second each time we are multiples of an int, 
   or a number of seconds before each revision. *)
let each_event_delay = int_of_float !color_delay;;
let every_n_events_delay = 
  let frac = !color_delay -. (floor !color_delay) in 
  if frac > 0.001
  then Some (max 1 (int_of_float (1. /. frac)))
  else None;;
  

(* Prepares the database connection information *)
let mediawiki_db = {
  dbhost = Some !mw_db_host;
  dbname = Some !mw_db_name;
  dbport = Some !mw_db_port;
  dbpwd  = Some !mw_db_pass;
  dbuser = Some !mw_db_user;
}

(* Here begins the sequential code *)

let mediawiki_dbh = Mysql.connect mediawiki_db in 
let db = Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh None 
  !mw_db_name !wt_db_rev_base_path !wt_db_blob_base_path !dump_db_calls in

(* If requested, we erase all coloring, and we recompute it from scratch. *)
if !delete_all then begin 
  db#delete_all true; 
  !Online_log.online_logger#log "\n  Cleared the db.\n"
end;

(* Creates an event processor *)
let processor = new Updater.updater 
  db trust_coeff !times_to_retry_trans each_event_delay every_n_events_delay 
  !robots 
in

(* Processes the event, as requested. *)
begin
  match !eval_type with
    VOTE -> begin
      let page_id = match !requested_page_id with 
	  None -> raise MissingInformation
	| Some d -> d
      in
      let revision_id = match !requested_rev_id with 
	  None -> raise MissingInformation
	| Some d -> d
      in
      let voter_name = match !requested_voter_name with 
	  None -> raise MissingInformation
	| Some d -> d
      in
      processor#eval_vote page_id revision_id voter_name
    end

  | EVENT -> begin
      processor#update_global
    end
end;
    
(* Closes the db connection *)
db#close;
    
(* Close the logger *)
!Online_log.online_logger#close

