(*

Copyright (c) 2009 Luca de Alfaro
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


open Online_command_line;;

(* Figure out what to do and how we are going to do it. *)
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: cat rep_file | load_reputations -db_user ...\n";;

(* Histogram of reputations *)
let histogram = Array.make 10 0. in

(* returns the next line of the input file *)
let get_line input =
  try 
    Some (input_line input) 
  with
      End_of_file -> None
in

let add_rep (time : float) (user_id : int) (old_bin : int) 
    (new_bin : int)
    (user_rep : float) 
    (user_name : string) (db : Online_db.db) : unit = 
  (* Writes the reputation increment. *)
  db#inc_rep user_id user_rep user_name;
  (* Updates the histogram, somehow. *)
  histogram.(new_bin) <- histogram.(new_bin) +. sqrt(user_rep)
in

let rec main (db : Online_db.db) =
  let line = get_line stdin in
    match line with 
      | Some l -> (
          (try 
             Scanf.sscanf l "%f %d %d %d %f %S" add_rep db
	         with Scanf.Scan_failure _ -> 
             Printf.fprintf stderr "Error reading line: %s\n" l
          );
          main db
        )
      | None -> (
	  (* Writes the histogram.  Note that this essentially
	     is a fake, assuming the wiki is large. *)
	  let median = Online_types.compute_reputation_median histogram in
	  db#write_histogram histogram median
	)
in

(* Prepares the database connection information *)
let mediawiki_db = {
  Mysql.dbhost = Some !mw_db_host;
  Mysql.dbname = Some !mw_db_name;
  Mysql.dbport = Some !mw_db_port;
  Mysql.dbpwd  = Some !mw_db_pass;
  Mysql.dbuser = Some !mw_db_user;
} in
let mediawiki_dbh = Mysql.connect mediawiki_db in 
let db = Online_db.create_db !use_exec_api !db_prefix mediawiki_dbh !mw_db_name
  !wt_db_rev_base_path !wt_db_blob_base_path 
  !max_uncompressed_blob_size !max_revs_per_blob !dump_db_calls in  
main db

