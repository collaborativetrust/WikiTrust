(*

Copyright (c) 2007-2008 The Regents of the University of California
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
let p = Unix.open_process_in "git show --pretty=format:\"%H\" @{0}";;
let version_str = input_line p;;
ignore (Unix.close_process_in p);;
(* Then passes this information to fileinfo *)
Fileinfo.make_info_obj version_str "";;

(* This is the top-level code of the wiki xml evaluation.  *)

let continue = ref false;; 
let working_dir = ref "/tmp/"
let set_working_dir s = working_dir := s
let src_dir = ref "/var/tmp/trust_src"
let set_src_dir_cmd s = src_dir := s
let input_files = ref Vec.empty;;
let set_input_files s = input_files := Vec.append s !input_files;;
let unzip_cmd = ref "gunzip -c"
let set_unzip_cmd s = unzip_cmd := s
let use_stdin = ref false
let do_dist_processing = ref false
let remote_host = ref "panoramix.cse.ucsc.edu"
let set_remote_host_cmd s = remote_host := s  
let remote_color_dir = ref "/home/ipye/work/colored_wikis/"
let set_remote_color_dir_cmd s = remote_color_dir := s  
let begin_url = ref "http://trust:slugsrule@panoramix.cse.ucsc.edu:801/get_file_by_expr.py?e=one&n=1"
let set_begin_url_cmd s = begin_url := s  
let done_url = ref "http://trust:slugsrule@panoramix.cse.ucsc.edu:801/mark_file_processed.py?e=one&file="
let set_done_url_cmd s = done_url := s   
let dist_client = ref "grab_files_to_color"
let set_dist_client_cmd s = dist_client := s  
let remote_user = ref ""
let set_remote_user_cmd s = remote_user := s  
let times_to_loop = ref 1
let set_times_to_loop_cmd t = times_to_loop := t
let loop_until_done = ref false
let single_out = ref ""
let set_single_out s = begin 
  use_stdin := true; 
  single_out := s
end

let factory = new Page_factory.page_factory 

let command_line_format = 
  [("-d", Arg.String set_working_dir, "<dir>: Directory where to put output file (default: /tmp)");
   ("-si", Arg.String set_single_out, "<file>: Uses stdin as input, and directs the output to the single file named <file>.  The [input_files] arguments are discarded");
   ("-unzip", Arg.String set_unzip_cmd, "<cmd>: Command to unzip the input wiki xml files (default: gunzip -c)");
   ("-continue", Arg.Set continue, "do not stop for errors on single input files");
   ("-do_dist", Arg.Set do_dist_processing, "Perform in distributed mode");
   ("-remote_host", Arg.String set_remote_host_cmd, "<host>: host where to get/put data files (default: panoramix.cse.ucsc.edu)");
   ("-remote_user", Arg.String set_remote_user_cmd, "<user>: user for remote host");
   ("-remote_color_dir", Arg.String set_remote_color_dir_cmd, "<dir>: Dir on the remote host to place finished colored wikis");
   ("-begin_url", Arg.String set_begin_url_cmd, "<url>: where to get files from");
   ("-done_url", Arg.String set_done_url_cmd, "<url>: where to go to mark a file  as processed");
   ("-dist_client", Arg.String set_dist_client_cmd, "<cmd>: client to interact with dist file metaserver (default: grab_files_to_color)");
   ("-times_to_loop", Arg.Int set_times_to_loop_cmd, "<number> how many times to loop getting a new batch of files to process, in distributed process mode (default 1)");
   ("-loop_until_done", Arg.Set loop_until_done, "do not stop until there are no more files to evaluate");
   ("-src_dir", Arg.String set_src_dir_cmd, "<dir> Where to place the src files downloaded during dist. processing -- must be different for each process");
  ] 
  @ factory#get_arg_parser

let _ = Arg.parse command_line_format set_input_files "Usage: evalwiki [input_files]";;

(* Does all the work *)

if !use_stdin then begin 
  let f_out = open_out !single_out in 
  Do_eval.do_single_eval factory stdin f_out
end else if !do_dist_processing 
then ignore (Do_eval.do_eval_dist !remote_host !remote_user !remote_color_dir !begin_url !done_url !dist_client !times_to_loop !loop_until_done factory !src_dir !working_dir !unzip_cmd !continue)
else ignore (Do_eval.do_multi_eval !input_files factory !working_dir !unzip_cmd !continue);;
 
