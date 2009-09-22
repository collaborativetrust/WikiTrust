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

open Online_types;;
open Mysql;;

(** This class updates the reputation, origin, and trust information.
    Various types of update are available:

    - Global update: chronologically processes all unprocessed revisions 
      and votes, bringing all up to date. 

    - Page update: chronologically processes all unprocessed revisions 
      and votes for a single page, bringing the page up to date. 

    - Immediate vote: processes a vote assuming that the revision 
      voted on has already been analyzed.

    We could also consider, in the future:

    - Immediate edit: processes an edit to a page, assuming that
      the revision that has been edited has already been analyzed. 

 *)

class updater
  (db: Online_db.db) 
  (trust_coeff: Online_types.trust_coeff_t)
  (n_retries: int)
  (each_event_delay: int)
  (every_n_events_delay: int option)
  (robots: Read_robots.robot_set_t)

  = object (self)

    (** Running total of number of processed events *)
    val mutable n_processed_events = 0
    (** Max n. of events to process in present run.  It is convenient to keep
	it as an objext variable, rather than have it clutter all
	parameter lists. *)
    val max_events_to_process = !Online_command_line.max_events_to_process


    (** [wait_a_bit] introduces a controlled delay to throttle the computation.
	The amount of delay is [each_event_delay] seconds per event, 
	and one additional second every [every_n_events_delay] events. *)
    method private wait_a_bit : unit =
      if each_event_delay > 0 then Unix.sleep (each_event_delay); 
      begin 
	match every_n_events_delay with 
	  Some d -> begin 
	    if (n_processed_events mod d) = 0 then Unix.sleep (1);
	  end
	| None -> ()
      end


    (** [evaluate_revision page_id r] evaluates revision [r].  The
	function is recursive, because if some past revision of the
	same page that falls within the analysis horizon is not yet
	evaluated and colored for trust, it evaluates and colors it
	first.  It assumes we have the page lock.  *)
    method private evaluate_revision (r: Online_revision.revision): unit =
      (* The work is done via a recursive function, because if some
	 past revision of the same page that falls within the analysis
	 horizon is not yet evaluated and colored for trust, it evaluates
	 and colors it first. *)
      let rec evaluate_revision_helper (r: Online_revision.revision): unit = 
	let rev_id = r#get_id in
	let page_id = r#get_page_id in 
	if max_events_to_process = 0 or 
	  n_processed_events < max_events_to_process then 
	    begin 
	      begin (* try ... with ... *)
		try 
		  !Online_log.online_logger#log (Printf.sprintf 
		    "\nEvaluating revision %d of page %d\n" rev_id page_id);
		  let page = new Online_page.page db page_id rev_id (Some r) 
		    trust_coeff n_retries robots None in
		  n_processed_events <- n_processed_events + 1;
		  if page#eval 
		  then !Online_log.online_logger#log (Printf.sprintf 
		    "\nDone revision %d of page %d\n" rev_id page_id)
		  else !Online_log.online_logger#log (Printf.sprintf 
		    "\nRevision %d of page %d was already done\n" 
		    rev_id page_id);
		with Online_page.Missing_trust r' -> 
		  begin
		    (* We need to evaluate r' first *)
		    (* This if is a basic sanity check only. It should
		       always be true. *)
		    if r'#get_id <> rev_id then 
		      begin 
			!Online_log.online_logger#log (Printf.sprintf 
			  "\nMissing trust info: we need first to evaluate revision %d of page %d\n" 
			  r'#get_id r'#get_page_id);
			evaluate_revision_helper r';
			self#wait_a_bit;
			evaluate_revision_helper r
		      end (* rev_id' <> rev_id *)
		  end (* with: Was missing trust of a previous revision *)
	      end (* End of try ... with ... *)
	    end
      in evaluate_revision_helper r
	   
	   
    (** [evaluate_vote page_id revision_id voter_id] evaluates the vote
	by [voter_id] on revision [revision_id] of page [page_id].
	It assumes that the revision has already been analyzed for trust, 
	otherwise, it does nothing. 
        It assumes we have the page lock.
     *)
    method private evaluate_vote (page_id: int) (revision_id: int) (voter_id: int) = 
      if max_events_to_process = 0 or 
	n_processed_events < max_events_to_process then 
	  begin 
	    !Online_log.online_logger#log (Printf.sprintf
	      "\nEvaluating vote by %d on revision %d of page %d" 
	      voter_id revision_id page_id); 
	    let page = new Online_page.page db page_id revision_id 
	      None trust_coeff n_retries robots None in
	    begin
	      try
		if page#vote voter_id then begin 
		  (* We mark the vote as processed. *)
		  db#mark_vote_as_processed revision_id voter_id;
		  n_processed_events <- n_processed_events + 1;
		  !Online_log.online_logger#log (Printf.sprintf 
		    "\nDone processing vote by %d on revision %d of page %d"
		    voter_id revision_id page_id)
		end
	      with Online_page.Missing_work_revision -> begin
		(* We mark the vote as processed. *)
		db#mark_vote_as_processed revision_id voter_id;
		!Online_log.online_logger#log (Printf.sprintf 
		  "\nVote by %d on revision %d of page %d not processed: no trust for page"
		  voter_id revision_id page_id)
	      end
	    end
	  end
	    
    (** [process_feed feed] processes the event feed [feed], taking care of:
	- acquiring the relevant page locks.
	- throttling the computation as required.
	- playing it nice with other parallel computation, implementing 
	  bounded overtake on pages, and terminating if lock wait increases 
          too much.
     *)
    method private process_feed (feed : Event_feed.event_feed) : unit =
      (* This hashtable is used to implement the load-sharing algorithm. *)
      let tried : (int, unit) Hashtbl.t = Hashtbl.create 10 in 
      let do_more = ref true in 
      while !do_more && (max_events_to_process = 0 or n_processed_events < max_events_to_process) do 
	begin 
	  (* This is the main loop *)
	  match feed#next_event with 
	    None -> do_more := false
	  | Some (event_timestamp, page_id, event) -> begin 
	      (* We have an event to process *)
	      (* Tracks execution time *)
	      let t_start = Unix.gettimeofday () in 
	      
	      (* Tries to acquire the page lock. 
		 If it succeeds, colors the page. 
		 
		 The page lock is not used for correctness: rather, it
		 is used to limit transaction parallelism, and to allow
		 revisions to be analyzed in parallel: otherwise, all
		 processes would be trying to analyze them in the same
		 order, and they would just queue one behind the next.
		 The use of these locks, along with the [tried]
		 hashtable, enforces bounded overtaking, allowing some
		 degree of out-of-order parallelism, while ensuring that
		 the revisions of the same page are tried in the correct
		 order.
		 
		 We set the timeout for waiting as follows. 
		 - If the page has already been tried, we need to wait on it, 
	           so we choose a long timeout. 
	           If we don't get the page by the long timeout, this means that
	           there is too much db lock contention (too many simultaneously
	           active coloring processes), and we terminate. 
	         - If the page has not been tried yet, we set a short timeout, 
	           and if we don't get the lock, we move on to the next 
		   revision. 

		 This algorithm ensures an "overtake by at most 1"
		 property: if there are many coloring processes active
		 simultaneously, and r_k, r_{k+1} are two revisions of a
		 page p, it is possible that a process is coloring r_k
		 while another is coloring a revision r' after r_k
		 belonging to a different page p', but this revision r'
		 cannot be past r_{k+1}.  *)
	      let already_tried = Hashtbl.mem tried page_id in 
	      let got_it = 
		if already_tried 
		then db#get_page_lock page_id Online_command_line.lock_timeout 
		else db#get_page_lock page_id 0 in 
	      (* If we got it, we can process the event *)
	      if got_it then begin 
		(* Processes page *)
		try
		  if already_tried then Hashtbl.remove tried page_id; 
		  begin 
		    match event with 
		      Event_feed.Revision_event r -> self#evaluate_revision r
		    | Event_feed.Vote_event (revision_id, voter_id) -> 
			self#evaluate_vote page_id revision_id voter_id
		  end;
		  db#release_page_lock page_id
		with e -> begin
		  db#release_page_lock page_id;
		  raise e
		end
	      end else begin 
		(* We could not get the lock.  
		   If we have already tried the page, this means we waited LONG time; 
		   we quit everything, as it means there is some problem. *)
		if already_tried 
		then begin
		  do_more := false;
		  !Online_log.online_logger#log (Printf.sprintf 
		    "\nWaited too long for lock of page %d; terminating.\n" page_id);
		  flush stdout;
		end
		else Hashtbl.add tried page_id ();
	      end; (* not got it *)
	      let t_end = Unix.gettimeofday () in 
	      !Online_log.online_logger#log (Printf.sprintf "\nAnalysis took %f seconds.\n" (t_end -. t_start));
	      flush stdout
	    end (* event that needs processing *)
	end done (* Loop as long as we need to do events *)

	
    (** [process_page_feed feed] processes the event feed [feed] for a page,
	assuming that the lock for the page has already been acquired.
     *)
    method private process_page_feed (feed : Event_feed.event_feed) : unit =
      let do_more = ref true in 
      while !do_more && (max_events_to_process = 0 or n_processed_events < max_events_to_process) do 
	begin 
	  (* This is the main loop *)
	  match feed#next_event with 
	    None -> do_more := false
	  | Some (event_timestamp, page_id, event) -> begin 
	      (* We have an event to process *)
	      match event with
	      | Event_feed.Revision_event r -> self#evaluate_revision r
	      | Event_feed.Vote_event (revision_id, voter_id) -> 
		  self#evaluate_vote page_id revision_id voter_id
	    end (* event that needs processing *)
	end done (* Loop as long as we need to do events *)
	

    (** [update_vote page_id revision_id voter_id] tries to get the page lock,
	and process a vote. *)
    method eval_vote (page_id: int) (revision_id: int) (voter_id: int) : unit =
    let got_it = db#get_page_lock page_id Online_command_line.lock_timeout in 
    if got_it then begin
      try
	self#evaluate_vote page_id revision_id voter_id;
	db#release_page_lock page_id
      with e -> begin
	db#release_page_lock page_id;
	raise e
      end
    end


    (** [update_page page_id] updates the page [page_id], analyzing in
	chronological order the revisions and votes that have not been
	analyzed yet.  This algorithm performs a fast evaluation, which is
        slightly less safe than the one done by the standard update_page
        method, in case the past trust information is incomplete.
        This method is especially advantageous when many revisions
        of a page must be evaluated. *)
    method update_page_fast (page_id: int) : unit = 
      (* Gets the lock gor the page. *)
      let got_it = db#get_page_lock page_id Online_command_line.lock_timeout in 
      if got_it then begin
	try 
	  (* Creates a feed for the page events. *)
	  let feed = 
	    new Event_feed.event_feed db (Some page_id) None n_retries in
	  (* Reads the page sigs, and the chunks, to build a running
	     information for the page. *)
    let (pinfo, bid, page_sigs, page_chunks) =
	    try
	      let (pinfo', bid') = db#read_page_info page_id in
        let page_sigs' = db#read_page_sigs page_id in
	      let page_chunks' = db#read_page_chunks page_id in
          (pinfo', bid', page_sigs', page_chunks')
      with Online_db.DB_Not_Found -> begin
        (* Initializes the page *)
        db#page_init page_id;
	      let (pinfo', bid') = db#read_page_info page_id in
        (pinfo', bid', ref [], [])
      end
    in
	  (* Creates a new writer for the page. *)
	  let writer = new Revision_writer.writer 
	    page_id (Some bid) None (Some db) true in
	  let running_info = {
	    Online_page.run_sigs = page_sigs;
	    Online_page.run_chunks = page_chunks;
	    Online_page.run_page_info = pinfo;
	    Online_page.run_writer = writer;
	  } in 
	  (* Loops over the feed and processes it. *)
	  let do_more = ref true in 
	  while !do_more && (max_events_to_process = 0 or 
	      n_processed_events < max_events_to_process) do 
	    begin 
	      (* This is the main loop *)
	      match feed#next_event with 
		None -> do_more := false
	      | Some (event_timestamp, page_id, event) -> begin 
		  (* We have an event to process *)
		  match event with
		  | Event_feed.Revision_event r -> begin
		      let rev_id = r#get_id in
		      !Online_log.online_logger#log (Printf.sprintf 
			"\nEvaluating revision %d of page %d\n" rev_id page_id);
		      let page = new Online_page.page db page_id rev_id (Some r)
			trust_coeff n_retries robots (Some running_info) in
		      n_processed_events <- n_processed_events + 1;
		      if page#eval 
		      then !Online_log.online_logger#log (Printf.sprintf 
			"\nDone revision %d of page %d\n" rev_id page_id)
		      else !Online_log.online_logger#log (Printf.sprintf 
			"\nRevision %d of page %d was already done\n" 
			rev_id page_id);
		    end
		  | Event_feed.Vote_event (rev_id, voter_id) -> begin
		      !Online_log.online_logger#log (Printf.sprintf
			"\nEvaluating vote by %d on revision %d of page %d" 
			voter_id rev_id page_id); 
		      let page = new Online_page.page db page_id rev_id None
			trust_coeff n_retries robots (Some running_info) in
		      if page#vote voter_id then begin
			(* We mark the vote as processed. *)
			db#mark_vote_as_processed rev_id voter_id;
			n_processed_events <- n_processed_events + 1;
			!Online_log.online_logger#log (Printf.sprintf 
			  "\nDone processing vote by %d on revision %d of page %d"
			  voter_id rev_id page_id)
		      end
		    end
		end (* Event that needs processing. *)
	    end done; (* While loop over events. *)
	  (* Writes the page information to disk. *)
	  db#write_page_info page_id running_info.Online_page.run_page_info;
	  db#write_page_chunks page_id running_info.Online_page.run_chunks;
	  db#write_page_sigs page_id page_sigs;
	  let open_page_blob_id = writer#close in
	  db#write_open_blob_id page_id open_page_blob_id;
	  db#commit;
	  db#release_page_lock page_id
	with e -> begin
	  db#release_page_lock page_id;
	  raise e
	end
      end
	    

    (** [update_page page_id] updates the page [page_id], analyzing in
	chronological order the revisions and votes that have not been
	analyzed yet. *)
    method update_page (page_id: int) : unit = 
      (* Gets the lock gor the page. *)
      let got_it = db#get_page_lock page_id Online_command_line.lock_timeout in 
      if got_it then begin
	try 
	  (* Creates a feed for the page events. *)
	  let feed = new Event_feed.event_feed db (Some page_id) None n_retries in
	  self#process_page_feed feed;
	  db#release_page_lock page_id
	with e -> begin
	  db#release_page_lock page_id;
	  raise e
	end
      end


    (** [update_global] updates the wikitrust information of a wiki, 
	in global chronological order. *)
    method update_global : unit =
      (* Creates a feed for the page events. *)
      let feed = new Event_feed.event_feed db None None n_retries in
      self#process_feed feed


  end  (* class *)


