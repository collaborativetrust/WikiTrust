(*

Copyright (c) 2009 The Regents of the University of California
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

(* This module runs as a daemon, polling a database table for pages
   (articles) to be brought up to date.  Whenever a new page to be
   processed is found, a new process is forked to handle this request.
   There is a limit to the number of concurrent child processes.

   To say that new work needs to be done, proceed as follows:

   Votes:
   * Insert the vote in the wikitrust_votes table.
   * Insert the page in the wikitrust_queue table.

   Edits:
   * Insert the page in the wikitrust_queue table.
   * Optionally, add the text to the wikitrust_text_cache table.
   
   Missing coloring:
   * Insert the page in the wikiturst_queue table.

 *)

open Online_command_line
open Online_types

(* evry batch corresponds to 50 revisions, so this will do 1000 at most. *)
let max_batches_to_do = 20
let max_concurrent_procs = 10
let sleep_time_sec = 1
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: dispatcher";;

(* Store the active sub-processes *)
let working_children = Hashtbl.create max_concurrent_procs

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
let trust_coeff = Online_types.get_default_coeff in

(* Delay throttling code.
   There are two types of throttle delay: a second each time we are multiples 
   of an int, or a number of seconds before each revision. *)
let each_event_delay = int_of_float !color_delay in
let every_n_events_delay = 
  let frac = !color_delay -. (floor !color_delay) in 
    if frac > 0.001
    then Some (max 1 (int_of_float (1. /. frac)))
    else None
in

(**
   [check_subprocess_termination page_id process_id]
   Wait for the process to stop before accepting more.
   This function cleans out the hashtable working_children, removing all of the 
   entries which correspond to child processes which have stopped working.
*)
let check_subprocess_termination (page_id: int) (process_id: int) = 
  let stat = Unix.waitpid [Unix.WNOHANG] process_id in
  begin
    match (stat) with
      (* Process not yet done. *)
    | (0,_) -> () 
	(* Otherwise, remove the process. *)
	(* TODO(Luca): release the db lock! *)
    | (_, Unix.WEXITED s) 
    | (_, Unix.WSIGNALED s) 
    | (_, Unix.WSTOPPED s) -> Hashtbl.remove working_children page_id
  end
in


(** [process_page page_id] is a child process that processes a page
    with [page_id] as id. *)
let process_page = 
  (* Every child has their own db. *)
  let child_db = new Online_db.db !db_prefix mediawiki_db None 
    !wt_db_rev_base_path !wt_db_sig_base_path !wt_db_colored_base_path 
    !dump_db_calls in
  (* And a new updater. *)
  let processor = new Updater.updater child_db !use_exec_api !use_wikimedia_api
    trust_coeff !times_to_retry_trans each_event_delay every_n_events_delay in
  (* Brings the page up to date.  This will take care also of the page lock. *)
  processor#update_page page_id;
  (* Marks the page as processed. *)
  child_db#mark_page_as_processed page_id
  (* End of page processing. *)
in


(** [dispatch_page pages] -> unit.  Given a list [pages] of page_ids to
    process, this function starts a new process which brings the page up
    to date.
 *)
let dispatch_page (pages : int list) =
  (* Remove any finished processes from the list of active child processes. *)
  Hashtbl.iter check_subprocess_termination working_children;
    (* [launch_processing page]
       Given a page_id, forks a child which brings the page up to date. *)
  let launch_processing page_id = begin
    (* If the page is currently being processed, does nothing. *)
    if not (Hashtbl.mem working_children page_id) then begin
      let new_pid = Unix.fork () in
      match new_pid with 
      | 0 -> begin
	  logger#log (Printf.sprintf "I'm the child\n Running on page %d\n" page_id); 
	  process_page page_id;
	end
      | _ -> begin
	  logger#log (Printf.sprintf "Parent of pid %d\n" new_pid);  
	  Hashtbl.add working_children p (new_pid)
	end
    end  (* if the page is already begin processed *)
  end in
  Hashtbl.iter launch_processing pages
in


(* 
   [main_loop]
   Poll to see if there is work to be done in the database table;
   if there is work to be done, does it. 
*)
let main_loop () =
  while true do
    if (Hashtbl.length working_children) >= max_concurrent_procs then begin
      (* Cleans up terminated children, if any *)
      Hashtbl.iter check_subprocess_termination working_children
    end else begin
      let pages_to_process = db#fetch_work_from_queue
	(max (max_concurrent_procs - Hashtbl.length working_children) 0) 
	!times_to_retry_trans
      in dispatch_page pages_to_process
    end;
    Unix.sleep sleep_time_sec;
    if !synch_log then flush Pervasives.stdout;
  done
in

main_loop ()
