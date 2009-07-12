(*

Copyright (c) 2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro

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


(* Event_feed works in one of three contexts:

   1. Local installation.
      Get the metadata _and_ text from the local database, 
      build a list of revisions, and return it.

   2. Remote installation. 
      The MW API has already pre-fetched the data.  We can
      thus query the local database, and proceed as above.

   3. WMF installation.
      The exec API is called, instead of the local db, to 
      figure out the list of revisions to process (the call
      to the exec API can happen in online_db). 
      Then, we read the revision text via the exec API, 
      and return the Online_revision.revision list. 

   So in summary, online_db must return an Online_revision.revision list
   with every revision already filled in with the text. 

   Online_page.ml should no longer read (uncolored) revision text.

   This allows us to batch together access to revision text, if needed.

 *)


open Eval_defs;;
open Online_types;;
open Online_db;;

(* This flag causes debugging info to be printed *)
let debug = true

(* n_events_to_read is how many events to read at once from the db. 
   Too small a number lowers efficiency; around 100 is a good compromise. *)
let n_events_to_read = 100

(** This is the type of an event that needs to be processed. *)
type event_t = 
    Revision_event of Online_revision.revision
  | Vote_event of int * int (* revision_id, voter_id *)

(** This is a time, a page_id, and an event.  The reason the page_id is 
    factored apart is that we need the page id for all types of events, 
    in order to grab the right locks. *)
type event_occurrence_t = float * int * event_t 

(** This class is used to build a feed of events that need to be processed
    in chronological order. 
    
    In [event_feed db requested_page_id requested_rev_id n_retries],
    [db] is the db handle used for access, and [n_retries] is the
    number of times one should retry a database transaction.  

    If a [requested_page_id] is specified, then all events belong to
    the same page.

    The optional parameter [requested_rev_id] mentions a revision that
    should be included in the feed; this is used to close holes in the
    processing, albeit in an approximate way. *)
class event_feed
  (db: Online_db.db)
  (use_exec_api: bool)
  (use_wikipedia_api: bool)
  (requested_page_id: int option)
  (requested_rev_id: int option) 
  (n_retries: int) 
  =
object (self) 

  (** This is a Vec of revisions to analyze.  When this is or gets empty,
      we need to get some more from the db *)
    val mutable revs : event_occurrence_t Vec.t = Vec.empty 
    (** This is a Vec of votes to analyze.  When this is or gets empty,
	we need to get some more from the db. *)
    val mutable votes : event_occurrence_t Vec.t = Vec.empty 
    (** These two variables are true if there may be more votes / revisions
	to analyze in the db, and they are false otherwise. *)
    val mutable there_are_more_revs  = true 
    val mutable there_are_more_votes = true

    (** Reads more revisions from the db, in chronological order, appending them 
	to the revs Vec. *)
    method private read_revs : unit = 
      if there_are_more_revs then begin 
	(* times_tried keeps track of how many times we have tried this transaction *)
	let times_tried = ref 0 in
	while !times_tried < n_retries do 
	  db#start_transaction;
	  begin (* try ... with ... *)
	    try
	      let rev_list =
		begin 
		  let last_colored = 
		    begin 
		      try Some (db#fetch_last_colored_rev_time requested_page_id)
		      with Online_db.DB_Not_Found -> None 
		    end
		  in
		  begin 
		    match last_colored with 
		      Some (last_timestamp, last_id) ->
			db#fetch_all_revs_after requested_page_id requested_rev_id
			  last_timestamp last_id n_events_to_read
		    | None -> db#fetch_all_revs requested_page_id n_events_to_read
		  end
		end
	      in 
	      (* Ok, we succeeded. *)
	      (* Sets the flag that indicates whether there may be more revisions *)
	      there_are_more_revs <- (List.length rev_list) >= n_events_to_read; 
	      (* The function f makes event_occurrence_t out of the revision list *)
	      let f r = ((Timeconv.time_string_to_float r.rev_timestamp), r.rev_page, 
	                 Revision_event (Online_revision.make_revision r db)) in 
	      revs <- Vec.concat revs (Vec.of_list (List.map f rev_list)); 
	      db#commit;
	      times_tried := n_retries; 
	    with Online_db.DB_TXN_Bad -> begin 
	      times_tried := !times_tried + 1; 
	      db#rollback_transaction;
	    end
	  end (* try ... with ... *)
	done
      end (* there are more revs *)
      
    (** Reads more votes from the db, in chronological order, appending them 
	to the votes Vec. *)
    method private read_votes : unit = 
      if there_are_more_votes then begin 
	(* times_tried keeps track of how many times we have tried this transaction *)
	let times_tried = ref 0 in
	while !times_tried < n_retries do 
	  db#start_transaction;
	  begin (* try ... with ... *)
	    try
	      let votes_list = 
		db#fetch_unprocessed_votes requested_page_id n_events_to_read 
	      in 
	      there_are_more_votes <- (List.length votes_list) >= n_events_to_read; 
	      (* f makes a vote into an event_occurrence_t *)
	      let f v = (Timeconv.time_string_to_float v.vote_time), v.vote_page_id, Vote_event (v.vote_revision_id, v.vote_voter_id) in 
	      votes <- Vec.concat votes (Vec.of_list (List.map f votes_list));
	      db#commit;
	      times_tried := n_retries
	    with Online_db.DB_TXN_Bad -> begin 
	      times_tried := !times_tried + 1; 
	      db#rollback_transaction;
	    end
	  end (* try ... with ... *)
	done
      end (* there are more votes *)

      
    (** [next_event] is the main method of the class: it gives the next 
	event to process in chronological order.  It returns None 
        when there is nothing more to be done. *)
    method next_event : event_occurrence_t option = 
      (* First, ensures that we have some revisions and votes to fetch *)
      if revs  = Vec.empty then self#read_revs; 
      if votes = Vec.empty then self#read_votes; 
      (* Then, we simply give the first one in chronological order *)
      if revs = Vec.empty then begin 
	if votes = Vec.empty 
	then None
	else begin 
	  (* There is a vote, but not a revision *)
	  let (event, more_events) = Vec.pop 0 votes in 
	  votes <- more_events; 
	  Some event
	end
      end else begin 
	(* revs not empty *)
	if votes = Vec.empty then begin 
	  (* revs not empty, votes empty *)
	  let (event, more_events) = Vec.pop 0 revs in 
	  revs <- more_events; 
	  Some event
	end else begin 
	  (* There are both votes and revs; selects the first in chron order *)
	  let vote = Vec.get 0 votes in 
	  let rev  = Vec.get 0 revs  in 
	  let (vote_time, _, _) = vote in 
	  let (rev_time,  _, _) = rev  in 
	  if vote_time < rev_time then begin 
	    (* Gives the vote *)
	    votes <- Vec.remove 0 votes; 
	    Some vote
	  end else begin 
	    (* Gives the rev *)
	    revs <- Vec.remove 0 revs; 
	    Some rev
	  end
	end  (* both votes and revs nonempty *)
      end (* revs not empty *)
      

  end (* class *)
