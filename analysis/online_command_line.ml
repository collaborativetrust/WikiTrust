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

(* Basic command line options and functions for the online analysis *)

open Online_types;;

(** This is a timeout for how long we wait for database locks. 
    If we wait longer than this, then the db is too busy, and we quit all work. 
    Notice that this provides an auto-throttling mechanism: if there are too many
    instances of coloring active at once, we won't get the lock quickly, and the 
    process will terminate. *)
let lock_timeout = 20
(** This is the max number of revisions to color in a single db connection. *)
let n_revs_color_in_one_connection = 200

(** Type on analysis to perform *)
type eval_type_t = EVENT | VOTE

let db_prefix = ref ""
let set_db_prefix d = db_prefix := d

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
let db_prefix = ref ""
let set_db_prefix d = db_prefix := d
let dump_db_calls = ref false

(* Base path for filesystem storage of revisions. *)
let wt_db_rev_base_path = ref None
let set_wt_db_rev_base_path s = wt_db_rev_base_path := Some s
(* Base path for filesystem storage of blobs. *)
let wt_db_blob_base_path = ref None
let set_wt_db_blob_base_path s = wt_db_blob_base_path := Some s
(* Maximum size of uncompressed blob *)
let max_uncompressed_blob_size = ref 10000000
(* Use the exec API in order to get the list of revisions to process, and their
   content. *)
(* Max n. of revisions in a blob *)
let max_revs_per_blob = ref 20
let use_exec_api = ref false
(* Use the WikiMedia API in order to get the list of revisions to process, and their
   content. *)
let use_wikimedia_api = ref false

(* Logging *)
let synch_log = ref false
let set_log_name d = begin
  let f = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o640 d in
  Online_log.online_logger := new Online_log.logger f !synch_log
end

(* Other paramiters *)
let noop s = ()
let delete_all = ref false
let reputation_speed = ref 1.
let set_reputation_speed f = reputation_speed := f
let requested_rev_id = ref None
let set_requested_rev_id d = requested_rev_id := Some d
let color_delay = ref 0.
let set_color_delay f = color_delay := f 
let max_events_to_process = ref 0
let set_max_events_to_process n = max_events_to_process := n
let times_to_retry_trans = ref 3
let set_times_to_retry_trans n = times_to_retry_trans := n
let dump_db_calls = ref false
let eval_type = ref EVENT
let set_vote () = eval_type := VOTE
let requested_voter_name = ref None
let set_requested_voter_name f = requested_voter_name := Some f
let requested_page_id = ref None
let set_requested_page_id f = requested_page_id := Some f
(* Robot set *)
let robots = ref Read_robots.empty_robot_set
let set_robots s = robots := Read_robots.read_robot_file s

(* API params *)
let target_wikimedia = ref "http://en.wikipedia.org/w/api.php"
let set_target_wikimedia t = target_wikimedia := t 
let user_id_server = ref "http://toolserver.org/~Ipye/UserName2UserId.php"
let set_user_id_server t = user_id_server := t 


(* Figure out what to do and how we are going to do it. *)
let command_line_format = 
  [
   ("-db_prefix", Arg.String set_db_prefix, "<string>: Database table prefix (default: none)");
   ("-db_user", Arg.String set_mw_db_user, "<string>: Mediawiki DB username (default: wikiuser)");
   ("-db_name", Arg.String set_mw_db_name, "<string>: Mediawiki DB name (default: wikidb)");
   ("-db_pass", Arg.String set_mw_db_pass, "<string>: Mediawiki DB password");
   ("-db_host", Arg.String set_mw_db_host, "<string>: Mediawiki DB host (default: localhost)");
   ("-db_port", Arg.Int set_mw_db_port,    "<int>: Mediawiki DB port
   (default: 3306)");
   ("-wiki_api", Arg.String set_target_wikimedia, "<string>: Mediawiki api to target for missing revs");
   ("-user_id_api", Arg.String set_user_id_server, "<string>: location of a tool which turns user_names into user_ids"); 
   ("-dump_db_calls", Arg.Set dump_db_calls, ": Writes to the db log all
 database calls.  This is very verbose; use only for debugging.");
   ("-rev_base_path", Arg.String set_wt_db_rev_base_path, "<string>: Filesystem base path for filesystem storage of revisions (default: revisions are stored in the db)");
   ("-blob_base_path", Arg.String set_wt_db_blob_base_path, "<string>: Filesystem base path for filesystem storage of blobs (default: blobs are stored in the db)");
   ("-max_blob_size", Arg.Set_int max_uncompressed_blob_size, "<int>: Maximum size of an individual uncompressed blob");
   ("-max_revs_per_blob", Arg.Set_int max_revs_per_blob, "<int>: Maximum number of revisions in the same blob (default: 20)");
   ("-rev_id",  Arg.Int set_requested_rev_id, "<int>: (optional) revision ID that we want to ensure it is colored");
   ("-sync_log", Arg.Set synch_log, ": Flush writes to the log immediately. This is very slow; use only for debugging.  This option must be used BEFORE the log_file option is used.");
   ("-log_file", Arg.String set_log_name, "<filename>: Logger output file (default: /dev/null)");
   ("-eval_vote", Arg.Unit set_vote, ": Just evaluate the given vote");
   ("-voter_name",  Arg.String set_requested_voter_name, "<string>: (optional) voter that we want to evaluate the vote of");
   ("-page_id",  Arg.Int set_requested_page_id, "<int>: (optional) page ID that we want to evaluate the vote on");
   ("-rep_speed", Arg.Float set_reputation_speed, "<float>: Speed at which users gain reputation; 1.0 for large wikis");
   ("-throttle_delay", Arg.Float set_color_delay, "<float>: Amount of time (on average) to wait between analysis of events.  This can be used to throttle the computation, not to use too many resources.");
   ("-n_events", Arg.Int set_max_events_to_process, "<int>: Max number of events to process (default: 0 = all outstanding events) "); 
   ("-times_to_retry_trans", Arg.Int set_times_to_retry_trans, "<int>: Max number of times to retry a transation if it fails (default: 3)."); 
   ("-robots", Arg.String set_robots, "<filename>: reads the robots from the specified file.");
   ("-use_exec_api", Arg.Set use_exec_api, "Use the exec API to access the WMF DB.");
   ("-use_wikimedia_api", Arg.Set use_wikimedia_api, "Use the WikiMedia API to access the WMF DB.");
   ("-delete_all", Arg.Set delete_all, ": Recomputes all reputations and trust from scratch.  BE CAREFUL!! This may take a LONG time for large wikis.");
  ]
