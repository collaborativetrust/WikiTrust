(*

Copyright (c) 2009-2010 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye, Bo Adler

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
   * Optionally, add the text to the text cache (in the revision and text
     tables).
   
   Missing coloring:
   * Insert the page in the wikitrust_queue table.

   Delete a page:
   * Insert the page in the wikitrust_queue table; use the
     special page title "XXX DELETE ME".

   Debug:
   ocamldebug -I `ocamlfind query mysql` -I `ocamlfind query sexplib` -I 'ocamlfind query vec' -I ../../analysis -I ../../batch/analysis [other options]

 *)

open Online_command_line;;
open Online_types;;
open Online_log;;

exception Option_error of string;;

let max_concurrent_procs = ref 1
let set_max_concurrent_procs m = max_concurrent_procs := m 
let sleep_time_sec = 1
let forever = ref true
let sig_finish = function _ -> forever := false


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
let keep_cached_text = ref false

(* Debugging *)
let min_rev_id = ref None
let set_min_rev_id m = min_rev_id := Some m
let single_threaded = ref false
let maxtime_reaper = ref 0.0
let maxtime_mainloop = ref 0.0
let maxtime_launcher = ref 0.0
let maxtime_fetchwork = ref 0.0
let maxtime_subprocess = ref 0.0
let sig_profileclean = function _ -> begin
    maxtime_reaper := 0.0;
    maxtime_mainloop := 0.0;
    maxtime_launcher := 0.0;
    maxtime_fetchwork := 0.0;
    maxtime_subprocess := 0.0;
    Online_db.profileclean ();
end

let custom_line_format = [
  ("-concur_procs", Arg.Int set_max_concurrent_procs, "<int>: Number of pages to process in parellel.");
  ("-global_db_prefix", Arg.String set_global_db_prefix, "<string>: All wiki Database table prefix (default: none)");
  ("-global_db_user", Arg.String set_global_db_user, "<string>: All wiki DB username (default: wikiuser)");
  ("-global_db_name", Arg.String set_global_db_name, "<string>: All wiki DB name (default: wikidb)");
  ("-global_db_pass", Arg.String set_global_db_pass, "<string>: All wiki DB password");
  ("-global_db_host", Arg.String set_global_db_host, "<string>: All wiki DB host (default: localhost)");
  ("-global_db_port", Arg.Int set_global_db_port,    "<int>: All wiki DB port (default: 3306)");
  ("-min_rev_id", Arg.Int set_min_rev_id, "<int>: The earliest revision ID to download");
  ("-keep_text_cache", Arg.Set keep_cached_text, ": Do not erase revision text downloaded from another MW instance.");
  ("-single_threaded_mode", Arg.Set single_threaded, "Run without forking -- for debugging");
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


(** [create_db] figures out what mode we are in and creates an
    online_db object of the right type *)

let create_db mediawiki_dbh global_dbh =
  begin
    if !use_wikimedia_api && !use_exec_api then
      raise (Option_error "ExecAPI and MediawikiAPI are illegal together");
    if (not !use_wikimedia_api) && (not !use_exec_api) then
      raise (Option_error "One of ExecAPI or MediawikiAPI must be specified");
    if !use_wikimedia_api then
      new Online_db.db_mediawiki_api !db_prefix mediawiki_dbh 
        global_dbh !mw_db_name !wt_db_rev_base_path !wt_db_blob_base_path 
        !dump_db_calls !keep_cached_text
    else
      Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh 
        global_dbh !mw_db_name !wt_db_rev_base_path !wt_db_blob_base_path 
        !dump_db_calls !keep_cached_text
  end
in

(**
   [profiling funcName startTime maxtimeRef]
   Calculate the difftime and see if it's larger than
   the previous maximum difftime.  If so, print a
   message out.
*)
let profiling funcName starttime maxtime =
  let endtime = Unix.gettimeofday () in
  let difftime = endtime -. starttime in
  if difftime > !maxtime then begin
      maxtime := difftime;
      !online_logger#debug 1
	  (Printf.sprintf "%s: new maxtime = %f.\n" funcName difftime);
  end;
in

(**
   [check_subprocess_termination page_id process_id]
   Wait for the process to stop before accepting more.
   This function cleans out the hashtable working_children, removing all of the 
   entries which correspond to child processes which have stopped working.
*)
let check_subprocess_termination (flags:Unix.wait_flag list) (process_id: int) =
    let num_flags = List.length flags in
    !online_logger#debug 9
      (Printf.sprintf "Dispatcher waiting for pid %d, %d flags\n"
	process_id num_flags);
    let (pid, status) =
      try Unix.waitpid flags process_id
      with Unix.Unix_error _ -> begin
	!online_logger#debug 9
	  (Printf.sprintf "waitpid error with %d children, %d flags\n"
	  (Hashtbl.length working_children) num_flags);
	(0, (Unix.WEXITED 0))
      end
    in
    if pid = 0 then () (* Process not yet done. *)
    else begin
      let msg =
	match (status) with
	| Unix.WEXITED s -> Printf.sprintf "exited with code %d" s
	| Unix.WSIGNALED s -> Printf.sprintf "killed with signal %d" s
	| Unix.WSTOPPED s -> Printf.sprintf "stopped with signal %d" s
      in
      try begin
	let find_page page_id (tpid, tdate) prev_ans =
	  if tpid = pid then (page_id, tdate) else prev_ans
	in
	let (pageid, started_on) = Hashtbl.fold find_page working_children (0, 0.0) in
	if pageid = 0 then raise Not_found;
	Hashtbl.remove working_children pageid;
	!online_logger#debug 2
	    (Printf.sprintf "Child %d finished, %s; %d children left\n"
	    pid msg (Hashtbl.length working_children));
	profiling "reaper" started_on maxtime_reaper;
	flush_all ();
      end with
      | Not_found -> begin
	  !online_logger#log
	      (Printf.sprintf "reaper: pid %d not in working_children .\n" pid);
      end;
    end
in

let check_subprocess_byhash (page_id: int) ((process_id: int), (started_on: float)) = 
    check_subprocess_termination [Unix.WNOHANG] process_id
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
  let child_db = create_db child_dbh child_global_dbh in
  (* first, delete the page id if requested; will recolor, as well *)
  if page_title = "XXX DELETE ME" then begin
    child_db#delete_page page_id;
    !online_logger#debug 3 (Printf.sprintf "Deleting pageId %d\n" page_id);
  end;

  let processed_well = ref false in
  let times_tried = ref 0 in
  while !forever && (not !processed_well) && (!times_tried < !times_to_retry_trans) do
    times_tried := !times_tried + 1;
    (try begin
      (* If I am using the WikiMedia API, I need to first download any new
         revisions of the page. *)
      if !use_wikimedia_api then 
        ignore( Wikipedia_api.download_page_from_id ~sid:!min_rev_id child_db page_id);
  
      (* Creates a new updater. *)
      let processor = new Updater.updater child_db
	trust_coeff !times_to_retry_trans each_event_delay every_n_events_delay 
	!robots in
	(* Brings the page up to date.  This will take care also of the page 
	   lock. *)
	!online_logger#debug 3
	  (Printf.sprintf "Processing pageId %d on try %d\n"
	  page_id !times_tried);
	processor#update_page_fast page_id;
	processed_well := true
    end with
    | Wikipedia_api.API_error_noretry e -> begin
	!online_logger#log
            (Printf.sprintf "Wikipedia_api Error: %s\n   On %d %s\n   Exc %s\n" 
              e page_id page_title (Printexc.to_string (Wikipedia_api.API_error_noretry 
              e)));
	times_tried := !times_to_retry_trans;
      end
    | Wikipedia_api.API_error e -> (
	!online_logger#log
            (Printf.sprintf "Wikipedia_api Error: %s\n   On %d %s\n   Exc %s\n" 
              e page_id page_title (Printexc.to_string (Wikipedia_api.API_error 
              e)));
      )
    | e -> begin  (* Handle everything else generically here. *)
        !online_logger#log
            (Printf.sprintf "Other Error: On %d %s\n   Exc %s\n   Stack Trace:\n%s"
                    page_id page_title (Printexc.to_string e)
                    (Printexc.get_backtrace ()) );
        let got_it = child_db#get_page_lock page_id Online_command_line.lock_timeout in
        if got_it then begin
          try
            child_db#delete_page page_id;
            child_db#release_page_lock page_id
          with e -> begin
            child_db#release_page_lock page_id;
            raise e
          end
        end
      end
    );
  done;
  if !times_tried >= !times_to_retry_trans then
    !online_logger#log (Printf.sprintf "Giving up on %d %s\n"
        page_id page_title);
  (* Marks the page as processed. *)
  child_db#mark_page_as_processed page_id;
  child_db#close; (* close dbh *)
  (* End of page processing. *)
  !online_logger#debug 3
      (Printf.sprintf "Done with %s.\n" page_title);
in


(* A queue of pages we've cached from the database that
 * need processing.  Used in next two functions. *)
let work_queue = ref [] in

(** [dispatch_page pages] -> unit.  Given a list [pages] of [(page_id,
    page_title)] to process, this function starts a new process which
    brings the page up to date.  *)
let dispatch_page (pages : (int * string) list) =
  (* [launch_processing page]
     Given a page_id, forks a child which brings the page up to date. *)
  let launch_processing (page_id, page_title) = begin
    if !single_threaded then process_page page_id page_title else begin
    (* If the page is currently being processed, does nothing. *)
    if not (Hashtbl.mem working_children page_id) then begin
      flush_all ();
      let new_pid = Unix.fork () in
      match new_pid with 
      | 0 -> begin
	  !online_logger#debug 3 (Printf.sprintf 
            "I'm the child: Running on page %s (%d)\n" page_title page_id); 
	  process_page page_id page_title;
	  !online_logger#debug 3 (Printf.sprintf 
            "I'm the child: Finished on page %s (%d)\n" page_title page_id); 
	  exit 0;
	end
      | _ -> begin
	  !online_logger#debug 3
	     (Printf.sprintf "Parent of pid %d, page %d\n" new_pid page_id);  
	  Hashtbl.add working_children page_id ((new_pid), (Unix.gettimeofday ()));
          Hashtbl.add working_pid2page_id new_pid (page_id, page_title) 
	end
    end else begin
      (* The page is already being processed (but probably
       * finished).  Throw it back on the queue. *)
      work_queue := ((page_id,page_title)::!work_queue);
    end
  end
  end in
  let starttime = Unix.gettimeofday () in
  List.iter launch_processing pages ;
  profiling "launcher" starttime maxtime_launcher;
in

(**
  * fetch_work () - local caching of work queue from DB.
  * We need to cache the work from the DB, because when there
  * are too many entries in the DB it takes 10+ seconds for the
  * DB to respond.
  *)
let fetch_work db =
  let rec workHead size head tail =
    if size = 0 then begin
      work_queue := tail;
      head
    end else begin
      match tail with
	| [] -> begin
		  work_queue := tail;
		  head;
		end;
	| hd::tl -> workHead (size-1) (hd::head) tl
    end
  in
  let slots = max
	  (!max_concurrent_procs - Hashtbl.length working_children) 0
  in
  if !forever && ((List.length !work_queue) < slots) then begin
    let starttime = Unix.gettimeofday () in
    let more_work = db#fetch_work_from_queue
      (10 * !max_concurrent_procs)
      !times_to_retry_trans !max_concurrent_procs
    in
    work_queue := List.append !work_queue more_work;
    profiling "fetchwork" starttime maxtime_fetchwork;
  end;
  workHead slots [] !work_queue
in


(* 
   [main_loop]
   Poll to see if there is work to be done in the database table;
   if there is work to be done, does it. 

   Need to assign 1 proc for each wiki
   Use this, but also get the list of availible spaces not used 
   Get the max of 1 + slack.

*)
let main_loop db =
  let counter = ref 0 in
  begin
    (* db#init_queue true; *)
    if !delete_all then db#delete_all ();
    while (!forever || ((List.length !work_queue) > 0)) && !counter < 1000 do
      let starttime = Unix.gettimeofday () in
      if (Hashtbl.length working_children) >= !max_concurrent_procs
	|| !counter >= 1000 then
	(* Wait until a child terminates. *)
        check_subprocess_termination [] 0
      else begin
        let starttime = Unix.gettimeofday () in
        Hashtbl.iter check_subprocess_byhash working_children;
        profiling "check_subprocess" starttime maxtime_subprocess;
	let worktodo = fetch_work db in
	dispatch_page worktodo;
	(* And sleep for a bit to give time for more stuff to get in queue *)
	if (List.length worktodo) < 1 then
	  Unix.sleep sleep_time_sec;
      end;
      profiling "mainloop" starttime maxtime_mainloop;
      counter := !counter + 1
    done;
    (* wait for remaining children before terminating *)
    while (Hashtbl.length working_children) > 0 do
      check_subprocess_termination [] 0
    done
  end;
  db#close;
in

Printexc.record_backtrace true ;
Sys.set_signal Sys.sigterm  (Sys.Signal_handle sig_finish) ;
Sys.set_signal Sys.sigint   (Sys.Signal_handle sig_finish) ;
Sys.set_signal Sys.sigusr1  (Sys.Signal_handle sig_profileclean) ;
while !forever do
  begin
    let mediawiki_dbh = Mysql.connect mediawiki_db in 
    let global_dbh = match !global_db_name with
      | Some _ -> Some (Mysql.connect global_db) 
      | None -> None
    in 
    let db = create_db mediawiki_dbh global_dbh in
    main_loop db ;
    db#close ;
  end
done;;

