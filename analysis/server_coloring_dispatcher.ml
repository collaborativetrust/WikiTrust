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

(* Figures out which pages to update, and starts them going. *)

open Printf
open Mysql
open Unix
open Online_command_line
open Wikipedia_api

let max_concurrent_procs = 10
let sleep_time_sec = 1
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: dispatcher";;

let working_children = Hashtbl.create max_concurrent_procs

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

(* Color the asked for revision. *)
let rec process_revs (page : int) (rev_ids : int list) =
  let revs = fetch_pages_and_revs rev_ids in
  let title = 
    match (List.hd revs) with
      | (p,l) -> Printf.printf "Got page titled %s\n" p.page_title; p
  in
    Unix.sleep sleep_time_sec;
    if !synch_log then flush Pervasives.stdout;
    ()
in

(* Start a new process going which actually processes the missing page. *)
let dispatch_page rev_pages = 
  let new_pages = Hashtbl.create (List.length rev_pages) in
  let is_new_page p =
    try ignore (Hashtbl.find working_children p); false with Not_found -> true
  in
 
  let set_revs_to_get (p,r) =
    if (is_new_page p) then (
      (
	let current_revs = try Hashtbl.find new_pages p with Not_found -> [] in
	  (Hashtbl.replace new_pages p (r::current_revs))
      )
    ) else ()
  in 
  let launch_processing p r = (
    let new_pid = Unix.fork () in
      match new_pid with 
	| 0 -> (
	    Printf.printf "I'm the child\n Running on page %d\n" p;
	    process_revs p r
	  )
	| _ -> (Printf.printf "Parent of pid %d\n" new_pid;  
		Hashtbl.add working_children p (new_pid)
	       )
  ) in
    List.iter set_revs_to_get rev_pages;
    Hashtbl.iter launch_processing new_pages
in

(* Poll to see if there is any more work to be done. *)
let rec main_loop () =
  if (Hashtbl.length working_children) >= max_concurrent_procs then (
    (* Wait for the processes to stop before accepting more *)
    let f k v = (
      let stat = Unix.waitpid [WNOHANG] v in
	match (stat) with
	  | (0,_) -> () (* Process not yet done. *)
	  | (_, WEXITED s) -> Hashtbl.remove working_children k (* Otherwise, remove the process. *)
	  | (_, WSIGNALED s) -> Hashtbl.remove working_children k
	  | (_, WSTOPPED s) -> Hashtbl.remove working_children k
    ) in
      Hashtbl.iter f working_children
  ) else (
    let revs_to_process = db # fetch_next_to_color 
      (max (max_concurrent_procs - Hashtbl.length working_children) 0) in
      dispatch_page revs_to_process
  );
  Unix.sleep sleep_time_sec;
  if !synch_log then flush Pervasives.stdout;
  main_loop ()
in

main_loop ()
