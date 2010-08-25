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

(** This function opens a file using a decompression algorithm. *)
let open_compressed_file (file_name: string) (unzip_cmd: string)
    : in_channel =
  let forked = ref false in
  let in_f = ref stdin in
  while not !forked do begin 
    forked := begin 
      try 
	in_f := Unix.open_process_in (unzip_cmd ^ " " ^ file_name); 
	true
      with Unix.Unix_error (Unix.EAGAIN, _, _) -> false 
    end
  end done; (* while *)
  (* Waits a bit before reading from the pipe *)
  Unix.sleep 1;
  !in_f

(** This function closes a compressed file. *)
let close_compressed_file (f: in_channel) =
  begin
    try ignore (Unix.close_process_in f) 
    with Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  end

(** [read_gzipped_file file_name] returns as a string the uncompressed 
    contents of file [file_name]. *)
let read_gzipped_file (file_name: string) : string option = 
  try 
    (* First, opens the file in regular mode. *)
    let file_opt = try Some (open_in file_name)
    with Sys_error _ -> None in
    begin
      match file_opt with
	None -> None
      | Some in_file -> begin
	  (* Checks the file length: if zero, then we return an empty string. *)
	  if (in_channel_length in_file) = 0 then Some ""
	  else begin
	    let f = Gzip.open_in_chan in_file in
	    let str_len = 8192 in 
	    let str = String.create str_len in
	    let buf = Buffer.create 8192 in 
	    let read_more = ref true in 
	    while !read_more do begin
	      let n_read = Gzip.input f str 0 str_len in 
	      if n_read = 0 then read_more := false
	      else Buffer.add_string buf (String.sub str 0 n_read)
	    end done;
	    Gzip.close_in f;
	    Some (Buffer.contents buf)
	  end
	end
    end
  with Gzip.Error s -> raise (Gzip.Error (s ^ " reading file: " ^ file_name))

(** [write_gzipped_file file_name l s] writes to the file [file_name] 
    the gzipped contents of string [s]. *)
let write_gzipped_file (file_name: string) (s: string) : unit =
  try
    let n = String.length s in 
    if n > 0 then begin
      let f = Gzip.open_out file_name in
      Gzip.output f s 0 n;
      Gzip.close_out f
    end else begin
      (* Zero length string: we write a zero length file. *)
      let f = open_out file_name in
      close_out f
    end
  with Gzip.Error s -> raise (Gzip.Error (s ^ " writing file: " ^ file_name))

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
let get_filename (base_path: string) (page_id: int) (rev_id: int option) 
    : (string * string list) =   
  let page_str = Printf.sprintf "%012d" page_id in 
  let rev_str = match rev_id with 
    | Some ri -> Printf.sprintf "%012d" ri 
    | None -> "" 
  in 
  let list_dirs = ref [base_path] in
  let file_name = ref base_path in 
  (* First, the page directories. *)
  for i = 0 to 3 do begin
    let s = String.sub page_str (i * 3) 3 in 
    file_name := !file_name ^ "/" ^ s;
    list_dirs := !list_dirs @ [!file_name]
  end done;
  (* Then, the revision directory *)
  match rev_id with 
  | Some ri -> (
      let s = String.sub rev_str 6 3 in 
      file_name := !file_name ^ "/" ^ s;
      list_dirs := !list_dirs @ [!file_name];
      (* Now all together *)
      file_name := !file_name ^ "/" ^ page_str ^ "_" ^ rev_str ^ ".gz";
      (!file_name, !list_dirs)
    )
  | None -> (!file_name, !list_dirs)
   

(** [write_revision base_name page_id rev_id s] writes to disk the
    revision text [s], individually, in compressed format, belonging
    to the revision [rev_id] of page [page_id], given the directory
    path [base_name].  Directories are created if they do not already
    exist. *)
let write_revision (base_name: string) (page_id: int) (rev_id: int) (s: string) : unit =
  let (f_name, dir_l) = get_filename base_name page_id (Some rev_id) in 
  (* Makes the directories *)
  let make_dir (d: string) = 
    begin try 
      Unix.mkdir d 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> () end
  in List.iter make_dir dir_l;
  (* Writes the revision *)
  write_gzipped_file f_name s

(** [read_revision base_name page_id rev_id] returns the text of revision
    [rev_id] of page [page_id], read individually from its own compressed
    file. *)
let read_revision (base_name: string) (page_id: int) (rev_id: int) 
    : string option =
  let (f_name, _) = get_filename base_name page_id (Some rev_id) in 
  read_gzipped_file f_name;;

(** [delete_all base_name] deletes all the tree of revisions rooted
    at base_path. *)
let delete_all (base_name: string) : Unix.process_status =
  Unix.system ("rm -rf " ^ base_name)

(** [delete_revision base_name page_id rev_id] deletes the text of revision
    [rev_id] of page [page_id]. 

    If rev_id is none, all revision of page_id are deleted.
*)
let delete_revision (base_name: string) (page_id: int) (rev_id: int option) =
  let (f_name, _) = get_filename base_name page_id rev_id in 
  match rev_id with 
  | Some ri -> (
      try
	Unix.unlink f_name
      with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    )
  | None -> let _ = delete_all f_name in
    ()

(* **************************************************************** *)
(* Unit tests. *)

if false then begin
  let not_null = function
      None -> "Error: revision not found."
    | Some x -> x
  in 
  write_revision "/tmp/alpha" 43 54 "Ho voglia di sushi";
  (* This works *)
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  (* This fails: revision not found *)
  print_string (not_null (read_revision "/tmp/alpha" 43 55));
  (* This works *)
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  delete_revision "/tmp/alpha" 43 (Some 54);
  (* This fails, as it has been deleted. *)
  print_string (not_null (read_revision "/tmp/alpha" 43 54));
  (* This works. *)
  write_revision "/tmp/alpha" 43 54 "Ho voglia di sushi";
  
  (* This works, first printing hello there, then printing nothing,
     then a 0. *)
  write_gzipped_file "/tmp/beta" "hello there";
  print_string (not_null (read_gzipped_file "/tmp/beta"));
  write_gzipped_file "/tmp/gamma" "";
  print_string (not_null (read_gzipped_file "/tmp/gamma"));
  print_int (String.length (not_null (read_gzipped_file "/tmp/gamma")));

  (* This will fail and raise an exception *)
  let f = open_out "/tmp/delta" in
  output_string f "bah";
  close_out f;
  print_string (not_null (read_gzipped_file "/tmp/delta"));
end
