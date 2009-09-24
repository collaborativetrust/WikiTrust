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


open Evaltypes;;
open Mysql;;

(* First, I want to figure out which version I am. *)
let p = Unix.open_process_in "git show --pretty=format:\"%H\" @{0}";;
let version_str = input_line p;;
ignore (Unix.close_process_in p);;
(* Then passes this information to fileinfo *)
Fileinfo.make_info_obj version_str "";;

(* Input defaults *)
let bucket_dir = ref ""
let user_file = ref ""
let db_prefix = ref ""
let noop s = ()

let command_line_format = 
  [("-buckets", Arg.Set_string bucket_dir, 
   "Directory where the stat buckets are.");
   ("-db_prefix", Arg.Set_string db_prefix, 
   "DB prefix for the wiki.");
   ("-user_sql_file", Arg.Set_string user_file, 
   "File where the user reputations are stored.");
]

let _ = Arg.parse command_line_format noop "Usage: batch_reputation_db\n"


