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

   Debug:
   ocamldebug -I `ocamlfind query mysql` -I `ocamlfind query sexplib` -I 'ocamlfind query vec' -I ../../analysis -I ../../batch/analysis [other options]

 *)

open Online_command_line
open Online_types

exception Timeout (* Child Process is taking too long *)

let child_timeout_sec = 60 * 60 * 4 (* Stop trying to process a page after 4 hours. *) 

(* evry batch corresponds to 50 revisions, so this will do 1000 at most. *)
let max_batches_to_do = 20
let max_concurrent_procs = ref 1
let set_max_concurrent_procs m = max_concurrent_procs := m 
let sleep_time_sec = 1
let memcached_host = ref "localhost"
let set_memcached_host h = memcached_host := h
let memcached_port = ref 11211
let set_memcached_port p = memcached_port := p
let render_last_rev = ref false

(* All-wiki DB *)
let global_db_user = ref "wikiuser"
let set_global_db_user u = global_db_user := u
let global_db_pass = ref ""
let set_global_db_pass p = global_db_pass := p
let global_db_name = ref None
let set_global_db_name d = global_db_name := Some d
let global_db_host = ref "localhost"
let set_global_db_host d = global_db_host := d
let global_db_port = ref 3306
let set_global_db_port d = global_db_port := d
let global_db_prefix = ref ""
let set_global_db_prefix d = global_db_prefix := d

let custom_line_format = [
  ("-concur_procs", Arg.Int set_max_concurrent_procs, "<int>: Number of pages to process in parellel.");
  ("-memcached_host", Arg.String set_memcached_host, "<string>: memcached server (default localhost)");
  ("-memcached_port", Arg.Int set_memcached_port, "<int>: memcached port (default 11211).");
  ("-render_last_rev", Arg.Set render_last_rev, "render the most current rev and save it in memcached.");
  ("-global_db_prefix", Arg.String set_global_db_prefix, "<string>: All wiki Database table prefix (default: none)");
  ("-global_db_user", Arg.String set_global_db_user, "<string>: All wiki DB username (default: wikiuser)");
  ("-global_db_name", Arg.String set_global_db_name, "<string>: All wiki DB name (default: wikidb)");
  ("-global_db_pass", Arg.String set_global_db_pass, "<string>: All wiki DB password");
  ("-global_db_host", Arg.String set_global_db_host, "<string>: All wiki DB host (default: localhost)");
  ("-global_db_port", Arg.Int set_global_db_port,    "<int>: All wiki DB port (default: 3306)");

] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: dispatcher";;

(* Store the active sub-processes *)
let working_children = Hashtbl.create !max_concurrent_procs
let working_pid2page_id = Hashtbl.create !max_concurrent_procs

(* Prepares the database connection information *)
let mediawiki_db = {
  Mysql.dbhost = Some !mw_db_host;
  Mysql.dbname = Some !mw_db_name;
  Mysql.dbport = Some !mw_db_port;
  Mysql.dbpwd  = Some !mw_db_pass;
  Mysql.dbuser = Some !mw_db_user;
}

(* Prepares the global (all-wiki) database connection information *)
let global_db = {
  Mysql.dbhost = Some !global_db_host;
  Mysql.dbname = !global_db_name;
  Mysql.dbport = Some !global_db_port;
  Mysql.dbpwd  = Some !global_db_pass;
  Mysql.dbuser = Some !global_db_user;
}

(* Sets up the db *)
let mediawiki_dbh = Mysql.connect mediawiki_db in 
let global_dbh = match !global_db_name with
  | Some _ -> Some (Mysql.connect global_db) 
  | None -> None
in 
let db = Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh 
  global_dbh !mw_db_name !wt_db_rev_base_path !wt_db_blob_base_path 
  !dump_db_calls in
let logger = !Online_log.online_logger in
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
let check_subprocess_termination (page_id: int) ((process_id: int), (started_on: float)) = 
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
  
(** Renders the last revision of the given page and puts it into memcached. *)
let render_rev (rev_id : int) (page_id : int) (db : Online_db.db) : unit =
  let (_, _, blob_id) = db#read_wikitrust_revision rev_id in
  let rev_text = db#read_colored_markup page_id rev_id blob_id in
  let raw_rendered_text = Wikipedia_api.render_revision rev_text in
  let rendered_text = Renderer.render raw_rendered_text in
  let cache = Memcached.open_connection !memcached_host !memcached_port in
    Memcached.add cache (Memcached.make_revision_text_key rev_id 
      !Online_command_line.mw_db_name) 
      rendered_text;
    Memcached.close_connection cache
in

(** Handle child process timeouts. *)
let sigalrm_handler = Sys.Signal_handle 
  (fun _ -> 
    raise Timeout
  ) 
in

(** [process_page page_id] is a child process that processes a page
    with [page_id] as id. *)
let process_page (page_id: int) (page_title: string) = 
  (* Every child has their own db. *)
  let child_dbh = Mysql.connect mediawiki_db in 
  let child_global_dbh = match !global_db_name with
    | Some _ -> Some (Mysql.connect global_db) 
    | None -> None
  in 
  let child_db = Online_db.create_db !use_exec_api !db_prefix child_dbh
    child_global_dbh !mw_db_name !wt_db_rev_base_path 
    !wt_db_blob_base_path !dump_db_calls 
  in
  (* Setup an alarm so we can timeout if taking too long *)
  let sigalrm_oldhandler = Sys.signal Sys.sigalrm sigalrm_handler in
  let sigalrm_reset () = Sys.set_signal Sys.sigalrm sigalrm_oldhandler in
  let pages_downloaded = ref 0 in
  let processed_well = ref false in
  let times_tried = ref 0 in
  try
    ignore (Unix.alarm child_timeout_sec);
    while (not !processed_well) && (!times_tried < !times_to_retry_trans) do
      times_tried := !times_tried + 1;
      (* If I am using the WikiMedia API, I need to first download any new
	 revisions of the page. *)
      (try Printexc.print (fun () -> 
	pages_downloaded := if !use_wikimedia_api then 
	  Wikipedia_api.download_page_from_id child_db page_id else 0
	;
    
	(* If pages have been downloaded, AND if the new_page_id doesn't 
	   match the old_page_id, remove all of the old info from the db 
	   and re-process with the new info. *)
	if !pages_downloaded > 0 then child_db#clear_old_info_if_pid_changed 
	  page_id page_title;
    
	(* Creates a new updater. *)
	let processor = new Updater.updater child_db
	  trust_coeff !times_to_retry_trans each_event_delay every_n_events_delay 
	  !robots in
	  (* Brings the page up to date.  This will take care also of the page 
	     lock. *)
	  processor#update_page_fast page_id;
	  (* Renders the last revision of this page and stores it in memcached. *)
	  if !render_last_rev then render_rev (db#get_latest_rev_id_from_id page_id) page_id db;
	  processed_well := true
	) () with
      | Wikipedia_api.API_error e -> (
	  Printf.eprintf "Wikipedia_api Error: %s\nOn %d %s\nExp%s\n" 
	    e page_id page_title (Printexc.to_string (Wikipedia_api.API_error 
	    e));
	)
      | Timeout -> raise Timeout
      | _ -> begin  (* Handle everything else generically here. *)
	  Printf.eprintf "Other Error: On %d %s\n" page_id page_title;
	  child_db#delete_revs_for_page page_id;
	end
      );
    done;
    sigalrm_reset ();
  with Timeout -> begin
    sigalrm_reset ();
    child_db#rollback_transaction;
    Printf.eprintf "Timeout on <%d>%s\n" page_id page_title;
  end
  | exc -> begin
    sigalrm_reset ();
    child_db#rollback_transaction;
    Printf.eprintf "Unhandled exception: %s, on <%d>%s\n"
      (Printexc.to_string exc) page_id page_title;
  end;
  (* Marks the page as processed. *)
  child_db#mark_page_as_processed page_id page_title !pages_downloaded;
  child_db#close; (* Release any locks still held. *)
  (* End of page processing. *)
  Printf.printf "Done with %s.\n" page_title; flush_all ();
  exit 0;
in


(** [dispatch_page pages] -> unit.  Given a list [pages] of [(page_id,
    page_title)] to process, this function starts a new process which
    brings the page up to date.  *)
let dispatch_page (pages : (int * string) list) =
  (* Remove any finished processes from the list of active child processes. *)
  Hashtbl.iter check_subprocess_termination working_children;
  (* [launch_processing page]
     Given a page_id, forks a child which brings the page up to date. *)
  let launch_processing (page_id, page_title) = begin
    (* If the page is currently being processed, does nothing. *)
    if not (Hashtbl.mem working_children page_id) then begin
      let new_pid = Unix.fork () in
      match new_pid with 
      | 0 -> begin
	  logger#log (Printf.sprintf 
            "I'm the child\n Running on page %s\n" page_title); 
	  process_page page_id page_title;
	end
      | _ -> begin
	  logger#log (Printf.sprintf "Parent of pid %d\n" new_pid);  
	  Hashtbl.add working_children page_id ((new_pid), (Unix.time ()));
          Hashtbl.add working_pid2page_id new_pid (page_id, page_title) 
	end
    end  (* if the page is already begin processed *)
  end in
  List.iter launch_processing pages
in


(* 
   [main_loop]
   Poll to see if there is work to be done in the database table;
   if there is work to be done, does it. 

   Need to assign 1 proc for each wiki
   Use this, but also get the list of availible spaces not used 
   Get the max of 1 + slack.

*)
let main_loop () =
  while true do
    if (Hashtbl.length working_children) >= !max_concurrent_procs then begin
      (* Cleans up terminated children, if any *)
      Hashtbl.iter check_subprocess_termination working_children
    end else begin
      let pages_to_process = db#fetch_work_from_queue
	(max (!max_concurrent_procs - Hashtbl.length working_children) 0) 
	!times_to_retry_trans !max_concurrent_procs
      in
      dispatch_page pages_to_process
    end;
    Unix.sleep sleep_time_sec;
    if !synch_log then flush Pervasives.stdout;
  done 
in

main_loop ()
