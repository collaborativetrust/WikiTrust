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

let input_dir = ref ""
let sorted_dir  = ref "SORTEDSTATS/"
let bucket_dir = ref ""
let outfile = ref ""
let digits_per_bucket = ref 5
let max_lines_in_mem = ref 1000000
let do_sorting = ref false
let use_dirs = ref false
let lines_in_cache = ref 0
let noop s = ()

let command_line_format = [
  ("-outfile", Arg.Set_string outfile,
  "write entire sorted output to this file (if empty, large output not produced)");
  ("-sorted_dir",  Arg.Set_string sorted_dir,
  "write split sorted files to this directory (defaults to ./SORTEDSTATS/)");
  ("-input_dir",  Arg.Set_string input_dir,
  "directory where the input files are");
  ("-bucket_dir", Arg.Set_string bucket_dir,
  "directory where the unsorted buckets are");
  ("-n_digits", Arg.Set_int digits_per_bucket,
  "number of digits that go into the same bucket (default: 5, or about a day)");
  ("-use_subdirs", Arg.Set use_dirs, 
  "Uses subdirectory structure for files to sort (default: false)");
  ("-skip_sorting", Arg.Set do_sorting,
  "Skips the sorting step of the files");
  ("-flush", Arg.Set_int max_lines_in_mem,
  "number of lines to read into memory before flushing to disk (default: 1000000)")
];;
  
  
let _ = Arg.parse command_line_format noop usage_message;;

(* There is Arg.Set but no Arg.Reset, hence this kludge *)
do_sorting := not !do_sorting;;

(* Hash table where lines are stored *)
let tempbuckets = Hashtbl.create 20000

(* Create a temporary working directory tree *)
try 
  Unix.mkdir !bucket_dir 0o750;
with e -> begin 
  print_string "Warning: bucket directory already exists.\n"; 
  flush stdout 
end;

(* Gets the list of files to sort *)
let file_list_f = Unix.open_process_in ("find " ^ !input_dir ^ " -name *" ^ filetype) in 
(* Waits a bit before reading from the pipe *)
Unix.sleep 3;

let flush_tmp () =
  (* Now writes the hashtable *)
  print_endline "Flushing";
  (* This function is iterated on the hash table *)
  let f k lines_list = 
    (* The key of the hashtable is the index of the file to open. *)
    (* We need to produce a filename. *)
    let (f_name, d_name) = 
      if !use_dirs then begin
	let s = Printf.sprintf "%08d.bkt" k in 
	let p1 = String.sub s 0 5 in
	let d = !bucket_dir ^ "/" ^ p1 in
	(d ^ "/" ^ s, d)
      end else (Printf.sprintf "%s/%08d.bkt" !bucket_dir k, "")
    in
    let file = 
      try 
	open_out_gen [Open_append] 0o640 f_name
      with Sys_error _ -> begin
	(* If we use subdirs, make sure the subdir exists. *)
	if !use_dirs then begin
	  try
	    Unix.mkdir d_name 0o750
	  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
	end;
	(* Opens the file *)
	open_out f_name
      end
    in
    let p (l: string) = begin output_string file l; output_string file "\n"; end in 
    List.iter p lines_list;
    close_out file;
  in 
  Hashtbl.iter f tempbuckets;
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
	  let k = idx2 - idx1 - !digits_per_bucket in
	  let time_piece = String.sub line (idx1 + 1) (k - 1) in
	  let bucketnum = int_of_string time_piece in
	  if Hashtbl.mem tempbuckets bucketnum then begin
	    let l = Hashtbl.find tempbuckets bucketnum in
	    Hashtbl.remove tempbuckets bucketnum;
	    Hashtbl.add tempbuckets bucketnum (line :: l)
	  end else Hashtbl.add tempbuckets bucketnum [line];
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


if !do_sorting then begin
(*
  (* Now sort each of the files *)
  for i = 1 to 10 do begin 
    let filename = !bucket_dir ^ (Printf.sprintf "/stats%04d.tstats" i) in 
    let outfilename = !sorted_dir ^ (Printf.sprintf "/stats%04d.sorted" i) in 
    let commandtext = ("sort -n -k 2,2 " ^ filename ^ " > " ^ outfilename) in
    print_string ("Sorting: " ^ filename ^ "\n"); flush stdout;
    ignore (Unix.system commandtext);
    ignore (Unix.system ("rm " ^ filename))
  end done;

  (* And concatenate all the sorted files together *)
  if !outfile <> "" then begin 
    print_string "Concatenating the result into a single file...\n"; flush stdout; 
    for i = 1 to !numbuckets do begin 
      let filename = !outdir ^ (Printf.sprintf "/stats%04d.sorted" i) in 
      sortedfilelist := (!sortedfilelist ^ " " ^ filename)
    end done;
    ignore (Unix.system ("cat " ^ !sortedfilelist ^ " > " ^ !outfile))
  end
 *)
end;;

