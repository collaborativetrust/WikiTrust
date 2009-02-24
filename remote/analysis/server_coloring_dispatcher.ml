(*

Copyright (c) 2008-2009 The Regents of the University of California
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
  This module runs as a deamon, polling every second for new requests.
  Requests are of two types, a vote and a coloring request.
  
  Whenever a new request is found, a new process is forked to handle this 
  request. Note that to ensure consistancy, there is page level locking, so 
  that there is never a situation where two child processes are working on the 
  same page simultaniously.  (Ian: please, improve your English spelling...)

  There is also a limit to the number of concurrent child processes.

  The child process take a request tuple (Ian: what is a request tuple?  Above 
  you talk simply about requests) and figures out what type of request 
  this is.
  
  If it is a coloring request, the revision text is downloaded from the 
  mediawiki api. (Ian: always?  Don't you try to get it from a dump if possible?)
  
  It then gives the request to the appropriate function, evaluate_revision or 
  evaluate_vote.
   
*)

open Printf
open Mysql
open Unix
open Online_command_line
open Wikipedia_api
open Online_db
open Online_types

let max_concurrent_procs = 10
let sleep_time_sec = 1
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: dispatcher";;

(* Store the active sub-processes *)
let working_children = Hashtbl.create max_concurrent_procs

(* Keep an in-memory cache of user-ids, because these are not accessible via 
   the mediawiki api *)
let max_id_cache_size = 1000

(* Two hash tables lets us do a rough version of LRU replacement. *)
let user_id_cache_front = Hashtbl.create max_id_cache_size
let user_id_cache_back = ref (Hashtbl.create max_id_cache_size)

(* Prepares the database connection information *)
let mediawiki_db = {
  dbhost = Some !mw_db_host;
  dbname = Some !mw_db_name;
  dbport = Some !mw_db_port;
  dbpwd  = Some !mw_db_pass;
  dbuser = Some !mw_db_user;
}

(* Here begins the sequential code *)
let db = new Online_db.db !db_prefix mediawiki_db None !dump_db_calls in
let logger = new Online_log.logger !log_name !synch_log in
let n_processed_events = ref 0 in
let trust_coeff = Online_types.get_default_coeff in

(* There are two types of throttle delay: a second each time we are multiples 
   of an int, or a number of seconds before each revision. *)
let each_event_delay = int_of_float !color_delay in
let every_n_events_delay = 
  let frac = !color_delay -. (floor !color_delay) in 
    if frac > 0.001
    then Some (max 1 (int_of_float (1. /. frac)))
    else None
in

(* Wait for the processes to stop before accepting more *)
let clean_kids k v = (
  let stat = Unix.waitpid [WNOHANG] v in
    match (stat) with
      | (0,_) -> () (* Process not yet done. *)
      | (_, WEXITED s) -> Hashtbl.remove working_children k (* Otherwise, remove the process. *)
      | (_, WSIGNALED s) -> Hashtbl.remove working_children k
      | (_, WSTOPPED s) -> Hashtbl.remove working_children k
) in

(* This is the function that evaluates a revision. 
   The function is recursive, because if some past revision of the same page 
   that falls within the analysis horizon is not yet evaluated and colored
   for trust, it evaluates and colors it first. 
 *)
let rec evaluate_revision (page_id: int) (rev_id: int): unit = 
  if !n_processed_events < !max_events_to_process then 
    begin 
      begin (* try ... with ... *)
	try 
	  logger#log (Printf.sprintf "Evaluating revision %d of page %d\n" 
			rev_id page_id);
	  let page = new Online_page.page db logger page_id rev_id trust_coeff !times_to_retry_trans in
	  n_processed_events := !n_processed_events + 1;
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
	      Some d -> begin 
		if (!n_processed_events mod d) = 0 then Unix.sleep (1);
	      end
	    | None -> ()
	  end; 

	with Online_page.Missing_trust (page_id', rev_id') -> 
	  begin
	    (* We need to evaluate page_id', rev_id' first *)
	    (* This if is a basic sanity check only. It should always be true *)
	    if rev_id' <> rev_id then 
	      begin 
		logger#log (Printf.sprintf "Missing trust info: we need first to evaluate revision %d of page %d\n" rev_id' page_id');
		evaluate_revision page_id' rev_id';
		evaluate_revision page_id rev_id
	      end (* rev_id' <> rev_id *)
	  end (* with: Was missing trust of a previous revision *)
      end (* End of try ... with ... *)
    end
in

(* This is the code that evaluates a vote *)
let evaluate_vote (page_id: int) (revision_id: int) (voter_id: int) = 
  if !n_processed_events < !max_events_to_process then 
    begin 
      logger#log (Printf.sprintf "Evaluating vote by %d on revision %d of page %d\n" voter_id revision_id page_id); 
      let page = new Online_page.page db logger page_id revision_id trust_coeff !times_to_retry_trans in 
      if page#vote voter_id then begin 
	n_processed_events := !n_processed_events + 1;
	logger#log (Printf.sprintf "Done revision %d of page %d\n" 
		      revision_id page_id);
      end;
      (* Waits, if so requested to throttle the computation. *)
      if each_event_delay > 0 then Unix.sleep (each_event_delay); 
      begin 
	match every_n_events_delay with 
	  Some d -> begin 
	    if (!n_processed_events mod d) = 0 then Unix.sleep (1);
	  end
	| None -> ()
      end; 
    end 
in 

(* 
   Returns the user id of the user name if we have it, 
   or asks a web service for it if we do not. 
*)
let get_user_id u_name =
  try Hashtbl.find user_id_cache_front u_name with Not_found -> (
    try Hashtbl.find !user_id_cache_back u_name with Not_found -> (
      let u_id = try db # get_user_id u_name with DB_Not_Found -> 
	get_user_id u_name logger
      in
	if Hashtbl.length user_id_cache_front >= max_id_cache_size then (
	  Hashtbl.clear !user_id_cache_back;
	  user_id_cache_back := (Hashtbl.copy user_id_cache_front);
	  Hashtbl.clear user_id_cache_front 
	);
	Hashtbl.add user_id_cache_front u_name u_id;
	u_id
    )
  )
in

(* Grab at most 50 revs of the given page, starting at the given
   timestamp and going forward in time. 
   
   Returns a list of rev_ids which are to be colored.
*)
let get_revs_from_api page_title page_id last_timestamp =
  let (wiki_page, wiki_revs) = 
    (* Retrieve a page and revision list from mediawiki. *)
    fetch_page_and_revs_after page_title last_timestamp logger in
    match wiki_page with
      | None -> (logger#log (Printf.sprintf "Failed for page %s\n" 
			       page_title);
		 raise Wikipedia_api.Http_client_error
		)
      | Some pp -> (
	  logger#log (Printf.sprintf "Got page titled %s\n" pp.page_title);
	  (* Write the new page to the page table. *)
	  db#write_page pp
	);
	  (* Add the revision to the revision table of the db. *)
	  let update_and_write_rev rev =
	    rev.revision_page <- page_id;
	    (* User ids are not given by the api, so we have to use the 
	       toolserver. *)
	    rev.revision_user <- (get_user_id rev.revision_user_text);
	    db#write_revision rev
	  in
	    (* Write the list of revisions to the db. *)
	    List.iter update_and_write_rev wiki_revs;
	    (* Finaly, retun a list of simple rev_ids *)
	    let get_id rev = rev.revision_id in
	      List.map get_id wiki_revs
in		

(* Given a revision, this function
   either colors it or else processes the vote on it.

   Also, if the text is missing, it is downloaded from the mediawiki api.
*)
let process_revs (page_id : int) (rev_ids : int list) (page_title : string)
    (rev_timestamp : string) (user_id : int) =
  (* This recursive inner function actually does the processing *)
  let rec do_processing (rev_id : int) = 
    (* I assume that a user cannot vote on an unprocessed revision here. *)
    if (db#revision_needs_coloring rev_id) then (
      let last_present_timestamp = 
	try 
	  db#get_latest_present_rev_timestamp page_id 
	with DB_Not_Found -> Wikipedia_api.default_timestamp 
      in
	
      (* List of revs present but not colored. 
	 This is calculated by first getting the list of all revs currently 
	 present not colored, then requesting from the mediawiki
	 api any remaining revs for the given page.
      *) 
      let revs_to_color = (db # get_revisions_present_not_colored page_id) @
	(get_revs_from_api page_title page_id last_present_timestamp) in
	(* Now that the data we need is present locally, evaluate the 
	   revisions for trust,reputation. *)
      let f rev = 
	evaluate_revision page_id rev
      in
	List.iter f revs_to_color;
	(* Sleep for a bit so that we don't get confused. *)
	Unix.sleep sleep_time_sec;
	if !synch_log then flush Pervasives.stdout;
	(* We color 50 revs at a time. If the target revision_id 
	   still isn't colored, keep going. *)
	if (db#revision_needs_coloring rev_id) then (
	  do_processing rev_id
	) else (
	  (* End evaluate revision part. *)
	  db # mark_rev_as_processed rev_id
	)
    ) else ( (* Vote! *)
      let process_vote v = (
	if v.vote_page_id == page_id then 
	  evaluate_vote page_id rev_id v.vote_voter_id;
	  db # mark_rev_as_unprocessed rev_id
      ) in
      let votes = db#fetch_unprocessed_votes !max_events_to_process in
	List.iter process_vote votes
    )
  in
    (* Process each revision in turn. *)
    List.iter do_processing rev_ids;
    logger#log (Printf.sprintf
		  "Finished processing page %s\n" page_title);	      
    exit 0 (* No more work to do, stop this process. *)
in

(* Given a list of revs to process, this function starts a new process
   going which either colores the revision text or else handles a vote.

   It groups the raw list of revs by page, and then forks a new 
   process to handle each page, up to the concurrant proc. limit.
*)
let dispatch_page rev_pages = 
  let new_pages = Hashtbl.create (List.length rev_pages) in
  let is_old_page p = Hashtbl.mem working_children p in
    (* If the revision is part of a page which is not
       currently being processed, add it to a list to process.

       Otherwise, change its state back to unprocessed, and try again with it
       soon. I feel that trying to pass it to the working child would be too 
       complicated at this point.
    *)
  let set_revs_to_get (r,p,title,time,uid) =
    logger#log (Printf.sprintf "page %d\n" p);
    if (not (is_old_page p)) then (
      let current_revs = try Hashtbl.find new_pages p with 
	  Not_found -> ([],title,time,uid) in
	(Hashtbl.replace new_pages p ((r::(let x,_,_,_  = 
					     current_revs in x)),
				      title,time,uid))
    ) else (
      db#mark_rev_as_unprocessed r
    )
  in 
    (* Given a page_id and a tuple, fork a child which processes
       all the revisions of this page.
    *)
  let launch_processing p (r,t,rt,uid) = (
    let new_pid = Unix.fork () in
      match new_pid with 
	| 0 -> (
	    logger#log (Printf.sprintf 
			  "I'm the child\n Running on page %d rev %d\n" p 
			  (List.hd r));
	    process_revs p r t rt uid
	  )
	| _ -> (logger#log (Printf.sprintf "Parent of pid %d\n" new_pid);  
		Hashtbl.add working_children p (new_pid)
	       )
  ) in
    (* Remove any finished processes from the list of active child
       processes
    *)
    Hashtbl.iter clean_kids working_children;

    (* Order the revs by page. *)
    List.iter set_revs_to_get rev_pages;

    (* Actually process the pages. *)
    Hashtbl.iter launch_processing new_pages
in

(* Poll to see if there is any more work to be done. *)
let main_loop () =
  while true do
    if (Hashtbl.length working_children) >= max_concurrent_procs then (
      Hashtbl.iter clean_kids working_children
    ) else (
      let revs_to_process = db#fetch_next_to_color 
	(max (max_concurrent_procs - Hashtbl.length working_children) 0) in
	dispatch_page revs_to_process
    );
    Unix.sleep sleep_time_sec;
    if !synch_log then flush Pervasives.stdout;
  done
in

main_loop ()
