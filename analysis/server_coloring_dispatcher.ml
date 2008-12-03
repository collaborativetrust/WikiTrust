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
let noop s = ()

let max_concurrent_procs = 10
let sleep_time_sec = 1

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
 ("-dump_db_calls", Arg.Set dump_db_calls, ": Writes to the db log all
 database calls.  This is very verbose; use only for debugging.");
 ]

let _ = Arg.parse command_line_format noop "Usage: dispatcher";;

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

(* Start a new process going which actually processes the missing page. *)
let dispatch_page (r,p) = (
  try ignore (Hashtbl.find working_children p) with Not_found -> (
    Hashtbl.add working_children p (Unix.open_process "ls");
    Printf.printf "Running on page %d rev %d\n" r p
  )
) in

(* Poll to see if there is any more work to be done. *)
let rec main_loop () =
  if (Hashtbl.length working_children) >= max_concurrent_procs then (
    (* Wait for the processes to stop before accepting more *)
    let f k v = (
      let stat = Unix.close_process v in
	match stat with
	  | WEXITED s -> Hashtbl.remove working_children k
	  | WSIGNALED s -> Hashtbl.remove working_children k
	  | WSTOPPED s -> Hashtbl.remove working_children k
    ) in
      Hashtbl.iter f working_children
  ) else (
    let revs_to_process = db # fetch_next_to_color 
      (max (max_concurrent_procs - Hashtbl.length working_children) 0) in
      List.iter dispatch_page revs_to_process;
  );
  Unix.sleep sleep_time_sec;
  flush Pervasives.stdout;
  main_loop ()
in

main_loop ()
