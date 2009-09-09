(*

Copyright (c) 2009 The Regents of the University of California
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

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"

(** [read_gzipped_file file_name] returns as a string the uncompressed 
    contents of file [file_name]. *)
let read_gzipped_file (file_name: string) : string option = 
  let str_len = 8192 in 
  let s = String.create str_len in
  let buf = Buffer.create 8192 in 
  let fopt = try
    Some (Gzip.open_in file_name)
  with Sys_error _ -> None in
  match fopt with
    Some f -> begin
      let read_more = ref true in 
      while !read_more do begin
	let n_read = Gzip.input f s 0 str_len in 
	if n_read = 0 
	then read_more := false
	else Buffer.add_string buf (String.sub s 0 n_read)
      end done;
      Gzip.close_in f;
      Some (Buffer.contents buf)
    end
  | None -> None

(** [write_gzipped_file file_name l s] writes to the file [file_name] 
    the gzipped contents of string [s], with compression level [l]. *)
let write_gzipped_file (file_name: string) (s: string) : unit =
  (* TODO: does not work with empty strings! *)
  let f = Gzip.open_out file_name in
  let n = String.length s in 
  if n > 0 then
    Gzip.output f s 0 n;
  Gzip.close_out f

(** [get_filename base_path page_id rev_id] computes the filename where
    a compressed revision is stored, given a base path for the file system, 
    a page id, and a revision id.  It returns a pair, consisting of:
    - The full filename of the revision.
    - the list of directories that may need to be made.
    We divide the tree so that the page tree has branching factor of at 
    most 1000.
    We divide the revision tree in at most 1000 directories, based on the digits
    345 of the revision id. 
 *)
let get_filename (base_path: string) (page_id: int) (rev_id: int) : (string * string list) =   
  let page_str = Printf.sprintf "%012d" page_id in 
  let rev_str  = Printf.sprintf "%012d" rev_id  in 
  let list_dirs = ref [base_path] in
  let file_name = ref base_path in 
  (* First, the page directories. *)
  for i = 0 to 3 do begin
    let s = String.sub page_str (i * 3) 3 in 
    file_name := !file_name ^ "/" ^ s;
    list_dirs := !list_dirs @ [!file_name]
  end done;
  (* Then, the revision directory *)
  let s = String.sub rev_str 6 3 in 
  file_name := !file_name ^ "/" ^ s;
  list_dirs := !list_dirs @ [!file_name];
  (* Now all together *)
  file_name := !file_name ^ "/" ^ page_str ^ "_" ^ rev_str ^ ".gz";
  (!file_name, !list_dirs)

(** [write_revision base_name page_id rev_id s] writes to disk the
    revision text [s], in compressed format, belonging to the revision
    [rev_id] of page [page_id], given the directory path [base_name].
    Directories are created if they do not already exist. *)
let write_revision (base_name: string) (page_id: int) (rev_id: int) (s: string) : unit =
  let (f_name, dir_l) = get_filename base_name page_id rev_id in 
  (* Makes the directories *)
  let make_dir (d: string) = 
    begin try 
      Unix.mkdir d 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> () end
  in List.iter make_dir dir_l;
  (* Writes the revision *)
  write_gzipped_file f_name s

(** [read_revision base_name page_id rev_id] returns the text of revision
    [rev_id] of page [page_id]. *)
let read_revision (base_name: string) (page_id: int) (rev_id: int) 
    : string option =
  let (f_name, _) = get_filename base_name page_id rev_id in 
  read_gzipped_file f_name;;

(** [delete_revision base_name page_id rev_id] deletes the text of revision
    [rev_id] of page [page_id]. *)
let delete_revision (base_name: string) (page_id: int) (rev_id: int) =
  let (f_name, _) = get_filename base_name page_id rev_id in 
  try
    Unix.unlink f_name
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(** [delete_all base_name] deletes all the tree of revisions rooted
    at base_path. *)
let delete_all (base_name: string) : Unix.process_status =
  Unix.system ("rm -rf " ^ base_name)

(* **************************************************************** *)
(* Unit tests. *)

if false then begin
  let not_null = function
      None -> "Error: revision not found."
    | Some x -> x
  in 
  write_revision "/tmp/alpha" 43 54 "Ho voglia di sushi";
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  print_string (not_null (read_revision "/tmp/alpha" 43 55));
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  delete_revision "/tmp/alpha" 43 54;
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  write_revision "/tmp/alpha" 43 54 "Ho voglia di sushi";
end
