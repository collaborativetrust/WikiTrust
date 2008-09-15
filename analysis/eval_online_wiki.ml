(*

Copyright (c) 2007-2008 The Regents of the University of California
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

open Printf
open Mysql

(** This is a timeout for how long we wait for database locks. 
    If we wait longer than this, then the db is too busy, and we quit all work. 
    Notice that this provides an auto-throttling mechanism: if there are too many
    instances of coloring active at once, we won't get the lock quickly, and the 
    process will terminate. *)
let lock_timeout = 20
(** This is the max number of revisions to color in a single db connection. *)
let n_revs_color_in_one_connection = 200

(** This is the top-level code of the wiki online xml evaluation. 
    This is used for testing only: *)

(* Mediawiki DB *)
let mw_db_user = ref "wikiuser"
let set_mw_db_user u = mw_db_user := u
let mw_db_pass = ref ""
let set_mw_db_pass p = mw_db_pass := p
let mw_db_name = ref "wikidb"
let set_mw_db_name d = mw_db_name := d
let mw_db_host = ref "localhost"
let set_mw_db_host d = mw_db_host := d
let mw_db_port = ref 3306
let set_mw_db_port d = mw_db_port := d

(* Wikitrust DB *)
let use_separate_dbs = ref false
let wt_db_user = ref "wikiuser"
let set_wt_db_user u = wt_db_user := u; use_separate_dbs := true
let wt_db_pass = ref ""
let set_wt_db_pass p = wt_db_pass := p; use_separate_dbs := true
let wt_db_name = ref "wikidb"
let set_wt_db_name d = wt_db_name := d; use_separate_dbs := true
let wt_db_host = ref "localhost"
let set_wt_db_host d = wt_db_host := d; use_separate_dbs := true
let wt_db_port = ref 3306
let set_wt_db_port d = wt_db_port := d; use_separate_dbs := true

(* Other paramiters *)
let log_name = ref "/dev/null"
let set_log_name d = log_name := d
let synch_log = ref false
let noop s = ()
let delete_all = ref false
let reputation_speed = ref 1.
let set_reputation_speed f = reputation_speed := f
let requested_rev_id = ref None
let set_requested_rev_id d = requested_rev_id := Some d
let color_delay = ref 0.
let set_color_delay f = color_delay := f 
let max_rev_to_color = ref 100
let set_max_rev_to_color n = max_rev_to_color := n
let times_to_retry_trans = ref 3
let set_times_to_retry_trans n = times_to_retry_trans := n
let dump_db_calls = ref false

(* Figure out what to do and how we are going to do it. *)
let command_line_format = 
  [
   ("-db_user", Arg.String set_mw_db_user, "<string>: Mediawiki DB username (default: wikiuser)");
   ("-db_name", Arg.String set_mw_db_name, "<string>: Mediawiki DB name (default: wikidb)");
   ("-db_pass", Arg.String set_mw_db_pass, "<string>: Mediawiki DB password");
   ("-db_host", Arg.String set_mw_db_host, "<string>: Mediawiki DB host (default: localhost)");
   ("-db_port", Arg.Int set_mw_db_port,    "<int>: Mediawiki DB port (default: 3306)");

   ("-wt_db_user", Arg.String set_wt_db_user, "<string>: Wikitrust DB username (specify only if the wikitrust db is different from the mediawiki db) (default: wikiuser)");
   ("-wt_db_name", Arg.String set_wt_db_name, "<string>: Wikitrust DB name (specify only if the wikitrust db is different from the mediawiki db) (default: wikidb)");
   ("-wt_db_pass", Arg.String set_wt_db_pass, "<string>: Wikitrust DB password (specify only if the wikitrust db is different from the mediawiki db)");
   ("-wt_db_host", Arg.String set_wt_db_host, "<string>: Wikitrust DB host (specify only if the wikitrust db is different from the mediawiki db) (default: localhost)");
   ("-wt_db_port", Arg.Int set_wt_db_port, "<int>: Wikitrust DB port (specify only if the wikitrust db is different from the mediawiki db) (default: 3306)");

   ("-rev_id",  Arg.Int set_requested_rev_id, "<int>: (optional) revision ID that we want to ensure it is colored");
   ("-log_file", Arg.String set_log_name, "<filename>: Logger output file (default: /dev/null)");
   ("-rep_speed", Arg.Float set_reputation_speed, "<float>: Speed at which users gain reputation; 1.0 for large wikis");
   ("-throttle_delay", Arg.Float set_color_delay, "<float>: Amount of time (on average) to wait between analysis of revisions.  This can be used to throttle the computation, not to use too many resources.");
   ("-n_revs", Arg.Int set_max_rev_to_color, "<int>: Max number of revisions to process (default: 100) "); 
   ("-times_to_retry_trans", Arg.Int set_times_to_retry_trans, "<int>: Max number of times to retry a transation if it fails (default: 3)."); 
   ("-dump_db_calls", Arg.Set dump_db_calls, ": Writes to the db log all database calls.  This is very verbose; use only for debugging.");
   ("-delete_all", Arg.Set delete_all, ": Recomputes all reputations and trust from scratch.  BE CAREFUL!! This may take a LONG time for large wikis.");
  ]

let _ = Arg.parse command_line_format noop "
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

let n_colored_revs = ref 0;;
let logger = new Online_log.logger !log_name !synch_log;;
let trust_coeff = Online_types.get_default_coeff;;
let f m n = !reputation_speed *. (Online_types.default_dynamic_rep_scaling n m) in 
trust_coeff.Online_types.dynamic_rep_scaling <- f;;

(* There are two types of throttle delay: a second each time we are multiples of an int, 
   or a number of seconds before each revision. *)
let each_revision_delay = int_of_float !color_delay;;
let every_n_revisions_delay = 
  let frac = !color_delay -. (floor !color_delay) in 
  if frac > 0.001
  then Some (max 1 (int_of_float (1. /. frac)))
  else None;;
  

(* This is the function that evaluates a revision. 
   The function is recursive, because if some past revision of the same page 
   that falls within the analysis horizon is not yet evaluated and colored
   for trust, it evaluates and colors it first. 
 *)
let rec evaluate_revision (db: Online_db.db) (page_id: int) (rev_id: int) : unit = 
  if !n_colored_revs < !max_rev_to_color then 
    begin 
      begin (* try ... with ... *)
	try 
	  Printf.printf "Evaluating revision %d of page %d\n" rev_id page_id;
	  let page = new Online_page.page db logger page_id rev_id trust_coeff !times_to_retry_trans in
	  if page#eval then begin 
	    n_colored_revs := !n_colored_revs + 1;
	    Printf.printf "Done revision %d of page %d\n" rev_id page_id;
	  end else begin 
	    Printf.printf "Revision %d of page %d was already done\n" rev_id page_id;
	  end;
	  (* Waits, if so requested to throttle the computation. *)
	  if each_revision_delay > 0 then Unix.sleep (each_revision_delay); 
	  begin 
	    match every_n_revisions_delay with 
	      Some d -> begin 
		if (!n_colored_revs mod d) = 0 then Unix.sleep (1);
	      end
	    | None -> ()
	  end; 

	with Online_page.Missing_trust (page_id', rev_id') -> 
	  begin
	    (* We need to evaluate page_id', rev_id' first *)
	    (* This if is a basic sanity check only. It should always be true *)
	    if rev_id' != rev_id then 
	      begin 
		Printf.printf "Missing trust info: we need first to evaluate revision %d of page %d\n" rev_id' page_id';
		evaluate_revision db page_id' rev_id';
		evaluate_revision db page_id rev_id
	      end (* rev_id' != rev_id *)
	  end (* with: Was missing trust of a previous revision *)
      end (* End of try ... with ... *)
    end;;


(* Does all the work of processing the given page and revision *)
let mediawiki_db = {
  dbhost = Some !mw_db_host;
  dbname = Some !mw_db_name;
  dbport = Some !mw_db_port;
  dbpwd  = Some !mw_db_pass;
  dbuser = Some !mw_db_user;
}
let wikitrust_db_opt = 
  if !use_separate_dbs 
  then Some { 
    dbhost = Some !wt_db_host;
    dbname = Some !wt_db_name;
    dbport = Some !wt_db_port;
    dbpwd  = Some !wt_db_pass;
    dbuser = Some !wt_db_user;
  }
  else None

 
(* If requested, we erase all coloring, and we recompute it from scratch. *)
if !delete_all then begin 
  let db = new Online_db.db mediawiki_db wikitrust_db_opt !dump_db_calls in 
  db#delete_all true; 
  db#close;
  Printf.printf "Cleared the db.\n"
end

(* [analyze_a_bunch n_revs_to_color] gets from the database at most 
   [n_revs_to_color] revisions, and analyzes them. 
   Why do we call this function many times, rather than just setting
   [n_revs_to_color] to the total we have to do? 
   Because mysql seems to like much better smaller queries, in terms 
   of speed. 
   The function returns [true] if it analyzed them all, and [false] if 
   the db says that there are fewer than the requested number of revisions
   to analyze, so that one more call to [analyze_a_bunch] is unnecessary. *)
let analyze_a_bunch (n_revs_to_color: int) : bool = 

  let db = new Online_db.db mediawiki_db wikitrust_db_opt !dump_db_calls in 
  (* Generates the list of revisions, in chronological order, since the last colored one. *)
  (* The obvious way would be to do a join, of the revisions which do NOT appear in the 
     colored table, sorted chronologically.  However, this can be quite inefficient for 
     large numbers of revisions.  So what we do is we retrieve the time t of the most recently
     colored revision, and then we pull from the db all revisions with time greater or 
     equal to t (equal, to handle revisions with the same timestamp). *)
  
  let revs = ref [] in
  let times_tried = ref 0 in
  while !times_tried < !times_to_retry_trans do 
    db#start_transaction Online_db.Both;
    begin (* try ... with ... *)
      try
	let r =
	  begin 
	    let last_colored = 
	      begin 
		try Some db#fetch_last_colored_rev_time
		with Online_db.DB_Not_Found -> None 
	      end
	    in
	    begin 
	      match last_colored with 
		Some (last_timestamp, last_id) -> begin 
		  match !requested_rev_id with 
		    None -> db#fetch_all_revs_after last_timestamp last_id n_revs_to_color
		  | Some r_id -> db#fetch_all_revs_including_after r_id last_timestamp last_id n_revs_to_color
		end
	      | None -> db#fetch_all_revs n_revs_to_color
	    end
	  end
	in 
	revs := r;
	db#commit Online_db.Both;
	times_tried := !times_to_retry_trans;
      with Online_db.DB_TXN_Bad -> begin 
	times_tried := !times_tried + 1; 
	db#rollback_transaction Online_db.Both;
	revs := []
      end
    end (* try ... with ... *)
  done;

  (* If we got fewer revisions than we asked, this means that there are 
     none more to analyze.  It is useful to remember this. *)
  let there_are_more_revs = (List.length !revs) >= n_revs_to_color in 

  (* Here begins the analysis algo, now that we know which revisions we must analyze *)
  (* This hashtable is used to implement the load-sharing algorithm. *)
  let tried : (int, unit) Hashtbl.t = Hashtbl.create 10 in 
  (* color_more_revisions is used to decide when to stop the loop. *)
  let color_more_revisions = ref true in 
  
  (* This function is iterated on the list of revisions to be colored.  
     r is a row describing a revision read from the database; it will be made into
     a revision inside color_revs. *)
  let color_revs r =
    if !color_more_revisions then 
      begin 
	let rev = Online_revision.make_revision r db in 
	let page_id = rev#get_page_id in 
	let rev_id  = rev#get_id in 
	
	(* Tracks execution time *)
	let t_start = Unix.gettimeofday () in 
	
	(* Tries to acquire the page lock. 
	   If it succeeds, colors the page. 
	   
	   The page lock is not used for correctness: rather, it is used to limit 
	   transaction parallelism, and to allow revisions to be analyzed in parallel: 
	   otherwise, all processes would be trying to analyze them in the same order, 
	   and they would just queue one behind the next. 
	   The use of these locks, along with the [tried] hashtable, enforces bounded 
	   overtaking, allowing some degree of out-of-order parallelism, while ensuring
	   that the revisions of the same page are tried in the correct order. 

	   We set the timeout for waiting as follows. 
	   - If the page has already been tried, we need to wait on it, so we choose a long timeout. 
	   If we don't get the page by the long timeout, this means that there is too much db 
	   lock contention (too many simultaneously active coloring processes), and we terminate. 
	   - If the page has not been tried yet, we set a short timeout, and if we don't get the lock,
	   we move on to the next revision. 
	   This algorithm ensures an "overtake by at most 1" property: if there are many coloring
	   processes active simultaneously, and r_k, r_{k+1} are two revisions of a page p, it is 
	   possible that a process is coloring r_k while another is coloring a revision r' after r_k 
	   belonging to a different page p', but this revision r' cannot be past r_{k+1}. 
	 *)
	let already_tried = Hashtbl.mem tried page_id in 
	let got_it = 
	  if already_tried 
	  then db#get_page_lock page_id lock_timeout 
	  else db#get_page_lock page_id 0 in 
	(* If we got it, we can color the page *)
	if got_it then begin 
	  (* Processes page *)
	  if already_tried then Hashtbl.remove tried page_id; 
	  evaluate_revision db page_id rev_id;
	  db#release_page_lock page_id;
	  if !n_colored_revs >= !max_rev_to_color then begin 
	    color_more_revisions := false;
	    Printf.printf "Colored as many pages as requested; terminating.\n";
	    flush stdout;
	  end
	end else begin 
	  (* We could not get the lock.  
	     If we have already tried the page, this means we waited LONG time; 
	     we quit everything, as it means there is some problem. *)
	  if already_tried 
	  then begin
	    color_more_revisions := false;
	    Printf.printf "Waited too long for lock of page %d; terminating.\n" page_id;
	    flush stdout;
	  end
	  else Hashtbl.add tried page_id ();
	end; (* not got it *)
	let t_end = Unix.gettimeofday () in 
	Printf.printf "Analysis took %f seconds.\n" (t_end -. t_start);
	flush stdout
      end (* for a revision r that needs to be colored *)
  in

  List.iter color_revs !revs;

  (* Closes the db connection *)
  db#close;
  (* Returns whether we should do more work, and whether there are more revisions to analyze in the db *)
  there_are_more_revs && !color_more_revisions
in (* end of analyze_a_bunch *)

(* This, finally, is the main loop *)
let do_more = ref true in 
while !do_more do begin 
  (* We do a bunch *)
  Printf.printf "Start the analysis of a bunch of size %d.\n" n_revs_color_in_one_connection;
  let there_are_more = analyze_a_bunch n_revs_color_in_one_connection in 
  do_more := (!n_colored_revs < !max_rev_to_color) && there_are_more
end done
