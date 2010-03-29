(*

Copyright (c) 2007-2008 The Regents of the University of California
Copyright (c) 2009 Google Inc
All rights reserved.

Authors: Luca de Alfaro, B. Thomas Adler, Vishwanath Raman, Ian Pye

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

(* First, I want to figure out which version I am. *)

let get_version_str () : string = "Unknown version"

let version_str = 
  try get_version_str ()
  with _ -> "Version not available"
;;

(* This is the top-level code of the wiki xml evaluation.  *)

let continue = ref false;; 
let working_dir = ref "/tmp/"
let set_working_dir s = working_dir := s
let input_files = ref Vec.empty;;
let set_input_files s = input_files := Vec.append s !input_files;;
let unzip_cmd = ref "gunzip -c"
let set_unzip_cmd s = unzip_cmd := s
let use_stdin = ref false
let do_dist_processing = ref false
let remote_host = ref ""
let set_remote_host_cmd s = remote_host := s  
let times_to_loop = ref 1
let set_times_to_loop_cmd t = times_to_loop := t
let dump_update_path = ref None
let set_dump_update_path (s: string) = dump_update_path := Some s
let loop_until_done = ref false
let single_out = ref ""
let set_single_out s = begin 
  use_stdin := true; 
  single_out := s
end
let stdout_mode = ref false

let factory = new Page_factory.page_factory 

let command_line_format = 
  [("-d", Arg.String set_working_dir, "<dir>: Directory where to put output file (default: /tmp)");
   ("-si", Arg.String set_single_out, "<file>: Uses stdin as input, and directs the output to the single file named <file>.  The [input_files] arguments are discarded");
   ("-stdin_stdout", Arg.Set stdout_mode, "Uses stdin as input and stdout as output");
   ("-unzip", Arg.String set_unzip_cmd, "<cmd>: Command to unzip the input wiki xml files (default: gunzip -c)");
   ("-continue", Arg.Set continue, "do not stop for errors on single input files");
   ("-dump_update_path", Arg.String set_dump_update_path, "<dir>: Path to the root directory of a tree of revisions to be analyzed, after the revisions in the main dump are analyzed.  The tree of revisions muse be in this form: for page xxxyyyzzzuuu, we can find in file <dir>/xxx/yyy/zzz/xxxyyyzzzuuu.gz any additional revisions, each enclosed in <revision> ... </revision> tags.");
  ] 
  @ factory#get_arg_parser

let _ = Arg.parse command_line_format set_input_files "Usage: evalwiki [input_files]";;

(* Does all the work *)

if !stdout_mode then Do_eval.do_single_eval factory stdin !dump_update_path stdout
else if !use_stdin then begin 
  let f_out = open_out !single_out in 
  Do_eval.do_single_eval factory stdin !dump_update_path f_out;
  close_out f_out
end else 
  Do_eval.do_multi_eval factory !input_files !dump_update_path !working_dir !unzip_cmd !continue;;
 

