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

(*
  This module runs as a daemon, polling every second for new requests.
  Requests are of two types, a vote and a coloring request.
  
  Whenever a new request is found, a new process is forked to handle this 
  request. Note that to ensure consistency, there is page level locking, so 
  that there is never a situation where two child processes are working on the 
  same page simultaneously. 

  There is also a limit to the number of concurrent child processes.

  The child process take a request tuple. This is a row from the 
  database table: revision_id, page_id, page_title, rev_time, user_id. 
  
  Vote and Coloring requests are separated as follows:
  Votes are those where there is colored markup for the requested revision.
  Coloring conversely are those requests where there is not already 
  colored markup. We assume that is it never the case where a user can 
  vote on a revision which has not been colored, and conversely a colored 
  revision will never be re-colored without being voted on.

  If it is a coloring request, the revision text is downloaded from the 
  mediawiki api if it is not present locally.
  
  It then gives the request to the appropriate function, evaluate_revision or 
  evaluate_vote.
   
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

(**
   [evaluate_revision page_id rev_id child_db n_processed_events]
   is the function that evaluates a revision. 
   The function is recursive, because if some past revision of the same page 
   that falls within the analysis horizon is not yet evaluated and colored
   for trust, it evaluates and colors it first. 
 *)
let rec evaluate_revision (page_id: int) (rev_id: int) (child_db : Online_db.db) 
    (n_processed_events : int)
    : unit = 
  begin (* try ... with ... *)
    try 
      logger#log (Printf.sprintf "Evaluating revision %d of page %d\n" 
		    rev_id page_id);
      let page = new Online_page.page child_db logger 
	page_id rev_id trust_coeff !times_to_retry_trans in
      if page#eval then begin 
	logger#log (Printf.sprintf "Done revision %d of page %d\n" 
	  rev_id page_id);
      end else begin 
	logger#log (
	  Printf.sprintf "Revision %d of page %d was already done\n" 
	    rev_id page_id);
      end;
      (* Waits, if so requested to throttle the computation. *)
      if each_event_delay > 0 then Unix.sleep (each_event_delay); 
      begin 
	match every_n_events_delay with 
	| Some d -> begin 
	    if (n_processed_events mod d) = 0 then Unix.sleep (1);
	  end
	| None -> ()
      end       
    with Online_page.Missing_trust (page_id', rev_id') -> 
      begin
	(* We need to evaluate page_id', rev_id' first *)
	(* This if is a basic sanity check only. It should always be true *)
	if rev_id' <> rev_id then 
	  begin 
	    logger#log (Printf.sprintf "Missing trust info: we need first to evaluate revision %d of page %d\n" rev_id' page_id');
	    evaluate_revision page_id' rev_id' child_db (n_processed_events + 1);
	    evaluate_revision page_id rev_id child_db (n_processed_events + 2)
	  end (* rev_id' <> rev_id *)
      end (* with: Was missing trust of a previous revision *)
  end (* End of try ... with ... *)
in

(**
   [evaluate_vote page_id revision_id voter_id child_db]
   This is the code that evaluates a vote 
*)
let evaluate_vote (page_id: int) (revision_id: int) (voter_id: int) (child_db : Online_db.db) = 
  logger#log (Printf.sprintf "Evaluating vote by %d on revision %d of page %d\n" 
    voter_id revision_id page_id); 
  let page = new Online_page.page child_db logger page_id revision_id trust_coeff 
    !times_to_retry_trans in 
    if page#vote voter_id then 
      logger#log (Printf.sprintf "User %d voted for revision %d of page %d.\n" 
	voter_id revision_id page_id)
in 


(** [process_revs page_id rev_ids page_title rev_timestamp user_id] 
    Returns unit.
    This is given a page and a list of revisions to work on.
    It successivly calls either evaluate_vote or evaluate_revision 
    for each revision.
*)
let process_revs (page_id: int) (page_title : string)
    (requests : revision_processing_request_t list) =
  
  (* Each child has its own database. *)
  (* TODO: need to fix paths for child_db *)
  let child_db = new Online_db.db !db_prefix mediawiki_db None None None None !dump_db_calls in
  (* This recursive inner function actually does the processing *)
  let rec do_processing (req : revision_processing_request_t) = 
    match req.req_request_type with
      Coloring -> begin
	try (* to recover from API errors *)
	  logger#log (Printf.sprintf "Working on coloring page %S\n" page_title);
	  (* Fetches all new revisions. *)
	  let last_rev_id = ref (
	    try child_db#get_latest_col_rev_id page_id 
	    with Online_db.DB_Not_Found -> 0)
	  in
	  let n_batches = ref 0 in
	  let do_more = ref true in
	  let new_rev_id_l = ref [] in
	  while !do_more do begin
	    let (rev_id_l, next_rev') = Wikipedia_api.get_revs_from_api 
	      page_id page_title !last_rev_id child_db logger 0 in
	    new_rev_id_l := !new_rev_id_l @ rev_id_l;
	    n_batches := !n_batches + 1;
	    match next_rev' with
	      None -> do_more := false
	    | Some i -> begin
		do_more := (!n_batches < max_batches_to_do);
		last_rev_id := i
	      end
	  end done;
	  (* Evaluate the revisions. *)
	  let f rev = evaluate_revision page_id rev child_db 0 in
	  List.iter f !new_rev_id_l;
	  if !synch_log then flush Pervasives.stdout;
	with Wikipedia_api.API_error -> ()
      end
    | Vote -> begin
	logger#log (Printf.sprintf "Working on vote for revision %d of page %S\n" 
	  req.req_revision_id page_title);
	evaluate_vote page_id req.req_revision_id req.req_requesting_user_id child_db;
	child_db#mark_rev_as_processed req.req_revision_id
      end
  in
  (* Process each revision in turn. *)
  List.iter do_processing requests;
  logger#log (Printf.sprintf
    "Finished processing page %s\n" page_title);	      
  exit 0 (* No more work to do, stop this process. *)
in


(* ---qui--- *)


(** [dispatch_page rev_pages] -> unit.  Given a list [rev_pages] of
    revisions to process (of type revision_processing_request_t), this
    function starts a new process going which either colores the
    revision text or else handles a vote.
    It groups the raw list of revs by page, and then forks a new 
    process to handle each page, up to the concurrant proc. limit.
 *)
let dispatch_page (rev_pages : revision_processing_request_t list) = 
  let new_pages = Hashtbl.create (List.length rev_pages) in
  let is_old_page p = Hashtbl.mem working_children p in
    (* 
       [set_revs_to_get request]
       If the revision is part of a page which is not
       currently being processed, add it to a list to process.

       Otherwise, change its state back to unprocessed, and try again with it
       soon. I feel that trying to pass it to the working child would be too 
       complicated at this point.
    *)
  let set_revs_to_get (req : revision_processing_request_t) =
    logger#log (Printf.sprintf "page %d\n" req.req_page_id);
    if (not (is_old_page req.req_page_id)) then (
      let current_revs = try Hashtbl.find new_pages req.req_page_id with 
	  Not_found -> [] in
	Hashtbl.replace new_pages req.req_page_id (req :: current_revs)
    ) else (
      db#mark_rev_as_unprocessed req.req_revision_id
    )
  in 
    (* 
       [launch_processing page requests]
       Given a page_id and a list of coloring requests, 
       fork a child which processes all the requests.
    *)
  let launch_processing p reqs = (
    let new_pid = Unix.fork () in
      match new_pid with 
	| 0 -> (
	    logger#log (Printf.sprintf 
			  "I'm the child\n Running on page %d rev %d\n" p 
			  (List.hd reqs).req_revision_id);
	    process_revs p ((List.hd reqs).req_page_title) reqs
	  )
	| _ -> (logger#log (Printf.sprintf "Parent of pid %d\n" new_pid);  
		Hashtbl.add working_children p (new_pid)
	       )
  ) in
    (* Remove any finished processes from the list of active child
       processes
    *)
    Hashtbl.iter check_subprocess_termination working_children;

    (* Order the revs by page. *)
    List.iter set_revs_to_get rev_pages;

    (* Actually process the pages. *)
    Hashtbl.iter launch_processing new_pages
in

(* 
   [main_loop]
   Poll to see if there is any more work to be done. 
   If there is, do it.
*)
let main_loop () =
  while true do
    if (Hashtbl.length working_children) >= max_concurrent_procs then (
      Hashtbl.iter check_subprocess_termination working_children
    ) else (
      let revs_to_process = db#fetch_next_revisions_to_color 
	(max (max_concurrent_procs - Hashtbl.length working_children) 0) in
	dispatch_page revs_to_process
    );
    Unix.sleep sleep_time_sec;
    if !synch_log then flush Pervasives.stdout;
  done
in

main_loop ()
