(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Gillian Smith, Luca de Alfaro

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

(* Combine stats files uses bucket sort to combine all of the statistics files in a specified directory.
 * It writes its output to a specified file. *)

let filetype = ".stats"

let usage_message = "Usage: combinestats [<directory>]\nAll *.stats files in <directory> will be sorted\n"

let dirname = ref ""
let outfile = ref ""
let outdir  = ref "SORTEDSTATS/"
let numbuckets = ref 2000
let max_lines_in_mem = ref 200000

let tempbuckets = Hashtbl.create 1750
let sortedfilelist = ref ""
  (* Get command line arguments *)
let set_dirname (s: string) = dirname := s
let set_outfile (s: string) = outfile := s
let set_outdir  (s: string) = outdir  := s
let set_numbuckets (s: string) = numbuckets := (int_of_string s)
let set_max_lines_in_mem (n) = max_lines_in_mem := n
let lines_in_cache = ref 0

let command_line_format = [
  ("-outfile", Arg.String (set_outfile),
  "write entire sorted output to this file (if empty, large output not produced)");
  ("-outdir",  Arg.String (set_outdir),
  "write split sorted files to this directory (defaults to ./SORTEDSTATS/)");
  ("-n", Arg.String (set_numbuckets),
  "number of buckets (default: 1750, limit: 9999)");
  ("-flush", Arg.Int (set_max_lines_in_mem),
  "number of lines to read into memory before flushing to disk (default: 5000)")
];;
  
  
let _ = Arg.parse command_line_format set_dirname usage_message;;

if !dirname = "" then Arg.usage command_line_format usage_message;;

(* Create a temporary working directory *)
try Unix.mkdir !outdir 0o740 with e -> begin print_string "Warning: output file directory already exists.\n"; flush stdout end;;

(* Create the temp files ("buckets") *)
for i = 1 to !numbuckets do
  let commandtext = "touch " ^ !outdir ^ (Printf.sprintf "/stats%04d.tstats" i) in 
  ignore (Unix.system commandtext)
done;;

(* Gets the list of files to sort *)
let file_list_f = Unix.open_process_in ("find " ^ !dirname ^ " -name *" ^ filetype) in 
(* Waits a bit before reading from the pipe *)
Unix.sleep 3;

let flush_tmp () =
  (* Now writes the hashtable *)
  print_endline "Flushing";
  for i = 1 to !numbuckets do begin 
    if Hashtbl.mem tempbuckets i then begin 
      let filename = !outdir ^ (Printf.sprintf "/stats%04d.tstats" i) in 
      let file = open_out_gen [Open_append] 0o640 filename in
      (* Gets all lines *)
      let lines_list = Hashtbl.find_all tempbuckets i in 
      let p (l: string) = begin output_string file l; output_string file "\n"; end in 
      List.iter p lines_list;
      close_out file;
    end
  end done;
  Hashtbl.clear tempbuckets;
  lines_in_cache := 0
in

(* This function processes one .stat file *)
let bucketize_file () = 
  (* raises End_of_file when we are done *)
  let filename = input_line file_list_f in
  (* Opens the file *)
  print_string ("Processing: " ^ filename ^ "\n"); flush stdout; 
  let infile = open_in (filename) in
  let file_todo = ref true in 
  while !file_todo do begin 
    let line = try 
      input_line infile
    with End_of_file -> begin
      file_todo := false;
      ""
    end
    in 
    if !file_todo then begin 
      try begin
	let idx1 = String.index line ' ' in
	if (String.sub line 0 idx1) <> "Page:" then begin
	  let idx2 = String.index_from line (idx1 + 1) ' ' in
	  let k = min 4 (idx2 - idx1 - 6) in
	  let time_piece = String.sub line (idx1 + 2) k in 
	  let timestamp = int_of_string (time_piece) in
	  let bound x = max 1 (min !numbuckets x) in 
 	  let bucketnum = bound timestamp in
	  Hashtbl.add tempbuckets bucketnum line;
	  lines_in_cache := !lines_in_cache + 1;
	  if !lines_in_cache >= !max_lines_in_mem then flush_tmp ()
	end
      end
      with Not_found | Invalid_argument _ | Failure _ -> ()
    end 
  end done; (* while loop over lines of file *)
  close_in infile
in 
(* Bucketizes all files *)
try
  while true do bucketize_file () done
with End_of_file -> ();
flush_tmp ();
ignore (Unix.close_process_in file_list_f);;

(* Now sort each of the files *)
for i = 1 to !numbuckets do begin 
  let filename = !outdir ^ (Printf.sprintf "/stats%04d.tstats" i) in 
  let outfilename = !outdir ^ (Printf.sprintf "/stats%04d.sorted" i) in 
  let commandtext = ("sort -n -k 2,2 " ^ filename ^ " > " ^ outfilename) in
  print_string ("Sorting: " ^ filename ^ "\n"); flush stdout;
  ignore (Unix.system commandtext);
  ignore (Unix.system ("rm " ^ filename))
end done;;

(* And concatenate all the sorted files together *)
if !outfile <> "" then begin 
  print_string "Concatenating the result into a single file...\n"; flush stdout; 
  for i = 1 to !numbuckets do begin 
    let filename = !outdir ^ (Printf.sprintf "/stats%04d.sorted" i) in 
    sortedfilelist := (!sortedfilelist ^ " " ^ filename)
  end done;
  ignore (Unix.system ("cat " ^ !sortedfilelist ^ " > " ^ !outfile))
end;;
