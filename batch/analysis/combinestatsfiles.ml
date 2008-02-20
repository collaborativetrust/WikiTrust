(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Gillian Smith

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
 * It writes its output to a specified file
 * USAGE:
 *     ARG1: stat file directory
 *     ARG2: output directory
 *     ARG3: number of buckets (optional, temporary)
 *)

let filetype = ".stats"

let usage_message = "Usage: combinestats [<directory>]\n<directory> containing stats files must be specified\n"

let dirname = ref ""
let outfile = ref ""
let outdir  = ref "SORTEDSTATS/"
let numbuckets = ref 1750

let tempbuckets = Hashtbl.create 1750

let sortedfilelist = ref ""
  
(* Get command line arguments *)
let set_dirname (s: string) =
  dirname := s

let set_outfile (s: string) =
  outfile := s

let set_outdir  (s: string) =
  outdir  := s
    
let set_numbuckets (s: string) =
  numbuckets := (int_of_string s)

let command_line_format = [("-outfile", Arg.String (set_outfile),
			    "write entire sorted output to this file (if empty, large output not produced)");
			   ("-outdir",  Arg.String (set_outdir),
			    "write split sorted files to this directory (defaults to ./SORTEDSTATS/)");
			   ("-n", Arg.String (set_numbuckets),
			    "number of buckets (default: 1750, limit: 9999)")]


let _ = Arg.parse command_line_format set_dirname usage_message;;
  

if !dirname = "" then
  Arg.usage command_line_format usage_message;;
    
(* Create a temporary working directory *)
try Unix.mkdir !outdir 0o740 with e -> begin print_string "Warning: output file directory already exists.\n"; flush stdout end;;

(* Create the temp files ("buckets") *)
for i = 1 to !numbuckets do
  let commandtext =
    if i < 10 then
      ("touch " ^ !outdir ^ "stats000" ^ (string_of_int i) ^ ".tstats")
    else if i < 100 then
      ("touch " ^ !outdir ^ "stats00" ^ (string_of_int i) ^ ".tstats")
    else if i < 1000 then
      ("touch " ^ !outdir ^ "stats0" ^ (string_of_int i) ^ ".tstats")
    else
      ("touch " ^ !outdir ^ "stats"  ^ (string_of_int i) ^ ".tstats")
  in
    ignore (Unix.system commandtext)
done;;

(* Open the stat file directory *)
let statfile_dir = Unix.opendir !dirname

 
(* Start looping through all the stat files *)
let rec process_directory () =
  let filename = Unix.readdir statfile_dir in
    (*If it's one of filetype -- NOTE: should replace this with regexp*)
    if (filename <> ".") && (filename <> "..")
			 && ((String.length filename) > ((String.length filetype) + 1))
			 && ((Str.string_after filename ((String.length filename) - (String.length filetype))) = filetype)
    then
      let infile = open_in (!dirname ^ filename) in
	(* Function for processing an individual file *)
      let rec process_file () =
	let line = input_line infile in
	let timestamp = float_of_string (List.nth (Str.split (Str.regexp "[ \t]+") line) 1) in
	let bound num =
	  if num < 1 then
	    1
	  else if num > !numbuckets then
	    !numbuckets
	  else
	    num
	in
	let bucketnum = bound ((int_of_float (timestamp/.100000.)) - 10000) in
	  (*let commandtext = ("echo " ^ line ^ " >> TEMPWORK/stats" ^ (string_of_int bucketnum) ^ ".tstats") in*)
	  begin
	    (*print_string ((string_of_float timestamp) ^ ": " ^ (string_of_int bucketnum) ^ "\n");*)
	    (*Unix.system commandtext;*)
	    Hashtbl.add tempbuckets bucketnum line;
	    process_file ()
	  end
      in
	begin
	  print_string ("Processing: " ^ filename ^ "\n");
	  flush stdout;
	  try process_file () with e -> close_in_noerr infile;
	    (* Print the contents of the hashtable to temp files *)
	    for i = 1 to !numbuckets do
	      let lines = Hashtbl.find_all tempbuckets i in
	      let filename =
		if i < 10 then
		  (!outdir ^ "stats000" ^ (string_of_int i) ^ ".tstats")
		else if i < 100 then
		  (!outdir ^ "stats00"  ^ (string_of_int i) ^ ".tstats")
		else if i < 1000 then
		  (!outdir ^ "stats0"   ^ (string_of_int i) ^ ".tstats")
		else
		  (!outdir ^ "stats"    ^ (string_of_int i) ^ ".tstats")
	      in
	      let file = open_out_gen [Open_append] 0o640 filename in
	      let rec print_list_to_file lst =
		if (List.length lst) = 0 then
		  ()
		else
		  begin
		    output_string file (List.hd lst);
		    output_string file "\n";
		    print_list_to_file (List.tl lst);
		  end
	      in
		begin
		  print_list_to_file lines;
		  close_out file;
		end
	    done;
	      (* Clear the hash table for the next file's info *)
	      Hashtbl.clear tempbuckets;
	      process_directory ();
	end
    else
      process_directory ();;


try process_directory () with e -> Unix.closedir statfile_dir;;



(* Now sort each of the files *)

for i = 1 to !numbuckets do
  let filename =
    if i < 10 then
      (!outdir ^ "stats000" ^ (string_of_int i) ^ ".tstats")
    else if i < 100 then
      (!outdir ^ "stats00"  ^ (string_of_int i) ^ ".tstats")
    else if i < 1000 then
      (!outdir ^ "stats0"   ^ (string_of_int i) ^ ".tstats")
    else
      (!outdir ^ "stats"    ^ (string_of_int i) ^ ".tstats")
  in    
  let outfilename =
    if i < 10 then
      (!outdir ^ "stats000" ^ (string_of_int i) ^ ".sorted")
    else if i < 100 then
      (!outdir ^ "stats00"  ^ (string_of_int i) ^ ".sorted")
    else if i < 1000 then
      (!outdir ^ "stats0"   ^ (string_of_int i) ^ ".sorted")
    else
      (!outdir ^ "stats"    ^ (string_of_int i) ^ ".sorted")
  in
  let commandtext = ("sort -n -k 2,2 " ^ filename ^ " > " ^ outfilename) in
    (*print_string (commandtext ^ "\n");*)
    begin
      print_string ("Sorting: " ^ filename ^ "\n");
      flush stdout;
      ignore (Unix.system commandtext);
      ignore (Unix.system ("rm " ^ filename))
    end
done;;

(* And concatenate all the sorted files together *)
  
for i = 1 to !numbuckets do
  let filename =
    if i < 10 then
      (!outdir ^ "stats000" ^ (string_of_int i) ^ ".sorted")
    else if i < 100 then
      (!outdir ^ "stats00"  ^ (string_of_int i) ^ ".sorted")
    else if i < 1000 then
      (!outdir ^ "stats0"   ^ (string_of_int i) ^ ".sorted")
    else
      (!outdir ^ "stats"    ^ (string_of_int i) ^ ".sorted")
  in
    sortedfilelist := (!sortedfilelist ^ " " ^ filename)
done;;

if !outfile <> "" then
  let _ = Unix.system ("cat " ^ !sortedfilelist ^ " > " ^ !outfile) in
    ();;
