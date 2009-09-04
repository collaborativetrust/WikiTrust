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

let _ = Arg.parse command_line_format noop "Usage: generate_reputation\n"

class usernamefinder (user_file: string) = object (self)

  val mutable id_to_name: (int, string) Hashtbl.t = Hashtbl.create 10000
    
  method private add_pair (id: int) (name: string) : unit =
    if id <> 0 && not (Hashtbl.mem id_to_name id)
    then Hashtbl.add id_to_name id name
      
  method add_data (datum: wiki_data_t) : unit = 
    match datum with 
      EditLife e -> begin
	self#add_pair e.edit_life_uid0 e.edit_life_uname0
      end
    | TextLife t -> begin
	self#add_pair t.text_life_uid0 t.text_life_uname0
      end
    | EditInc e -> begin
	self#add_pair e.edit_inc_uid0 e.edit_inc_uname0;
	self#add_pair e.edit_inc_uid1 e.edit_inc_uname1;
	self#add_pair e.edit_inc_uid2 e.edit_inc_uname2
      end
    | TextInc t -> begin
	self#add_pair t.text_inc_uid0 t.text_inc_uname0;
	self#add_pair t.text_inc_uid1 t.text_inc_uname1
      end
	
  method produce_sql : unit =
    let sql_file = open_out user_file in
    let write_pair id name =
      let id_sql = ml2int id in
      let name_sql = ml2str name in
      Printf.fprintf sql_file "INSERT INTO %swikitrust_user (user_id, username) VALUES (%s, %s) ON DUPLICATE KEY UPDATE username = %s;\n" !db_prefix id_sql name_sql name_sql
    in
    Hashtbl.iter write_pair id_to_name;
    close_out sql_file
    
end (* class usernamefinder *)
  
(* This is the reputation evaluator *)
let r = new usernamefinder !user_file;;

(* Reads the data, and passes it to the function that finds the
   username - user_id correspondences. *)

(* Gets all the bucket names (they can be many!) *)
let file_list_f = Unix.open_process_in ("find " ^ !bucket_dir) in 
Unix.sleep 3; (* waits a bit for the pipe to be set up *)
(* Reads all file names *)
let file_names_l = ref [] in
try
  while true do
    file_names_l := (input_line file_list_f) :: !file_names_l
  done
with End_of_file -> ();
ignore (Unix.close_process_in file_list_f);

(* Sorts the list *)
let file_names_a = Array.of_list !file_names_l in
let cmp s1 s2 = begin
  if s1 > s2 then 1
  else if s1 < s2 then -1 else 0
end in
Array.sort cmp file_names_a;

for file_idx = 0 to (Array.length file_names_a) - 1 do begin
  (* Checks if there is a sorted version of the same file *)
  let s = file_names_a.(file_idx) in
  let is_there_sorted_version = 
    file_idx < (Array.length file_names_a) - 1 
    && begin
      let s' = file_names_a.(file_idx + 1) in 
      let l = String.length s in
      let l' = String.length s' in
      l < l' && s = String.sub s' 0 l
    end
  in
  if not is_there_sorted_version then begin
    (* Ok, there is no better version for this file.
       Processes it. *)
    print_string ("Processing file " ^ s ^ "\n"); flush stdout;
    let stream = open_in s in 
    ignore (Wikidata.read_data stream r#add_data None);
    close_in stream
  end
end done;;

(* And prints the results *)
r#produce_sql;;

