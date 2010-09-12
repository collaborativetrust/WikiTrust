(*

Copyright (c) 2009-2010 Luca de Alfaro
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

(* The format of a blob is as follows: 

   header:binary_blob_content

   The header is guaranteed not to contain ":", so to find the start of the
   binary blob content, one can simply look for the ":". 

   The header is a list of (rev_id, rev_start, rev_len) items 
   (thus, in Ocaml, it has type (int * int * int) list)
   where: 
   - rev_id is the revision id
   - rev_start is the offset in the binary_blob_content
   - rev_len is the length, in bytes, in the binary_blob_content
 *)

open Online_types;;
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib

type blob_header_t = (int * int * int) list with sexp
let separator_char = ':'
let compress_prefix = "wikitrust_"
exception Invalid_blob_format
exception Compression_error

(** [get_filename base_path page_id blob_id] computes the filename where
    the compressed blob [blob_id] of page [page_id] is stored.
    It returns a tuple, consisting of:
    - The full filename of the revision.
    - The full filename of a temp file that can be used to ensure atomic write.
    - The list of directories that may need to be made.
    - The base directory where all the blobs for the page are stored.

    We divide the tree so that the page tree has branching factor of
    at most 1000.  
    If the blob id less than 1000, that is all there is: there are no
    blob-level directories, to optimize for the common case.
    If there are more than 1000 blobs, we then add xyz directories, 
    based on digits 345 of the blob id, for the pages that have very
    many revisions. *)
let get_filename (base_path: string) (page_id: int) (blob_id: int) 
    : (string * string * string list * string) =   
  let page_str = Printf.sprintf "%012d" page_id in 
  let blob_str  = Printf.sprintf "%09d" blob_id  in 
  let list_dirs = ref [base_path] in
  let path_name = ref base_path in 
  (* First, the page directories. *)
  for i = 0 to 3 do begin
    let s = String.sub page_str (i * 3) 3 in 
    path_name := !path_name ^ "/" ^ s;
    list_dirs := !list_dirs @ [!path_name]
  end done;
  (* We keep track of the base directory for the page. *)
  let page_dir = !path_name in
  (* Then, we add the blob directory *)
  if blob_id > 999 then begin
    let s = String.sub blob_str 0 6 in 
    path_name := !path_name ^ "/" ^ s;
    list_dirs := !list_dirs @ [!path_name]
  end;
  (* Now all together *)
  path_name := !path_name ^ "/" ^ page_str ^ "_" ^ blob_str;
  let file_name = !path_name ^ ".gz"; in
  let temp_name = !path_name ^ ".tmp.gz" in
  (file_name, temp_name, !list_dirs, page_dir)

(** [delete_all_page_blobs base_path page_id] removes all the blobs
    of a given page, including the sig blobs.  This can be useful
    to restart the analysis of a page from scratch. *)
let delete_all_page_blobs (base_path: string) (page_id: int) : 
    Unix.process_status =
  let (_, _, _, base_dir) = get_filename base_path page_id 0 in
  Unix.system ("rm -rf " ^ base_dir)

(* **************************************************************** *)
(* Filesystem blob access. *)

(** [write_blob base_path page_id blob_id s] writes to disk the
    blob with id [blob_id] and text [s], belonging to [page_id],
    given the directory path [base_path].
    Directories are created if they do not already exist. *)
let write_blob (base_path: string) (page_id: int) (blob_id: int) 
    (s: string) : unit =
  let (f_name, temp_name, dir_l, _) = get_filename base_path page_id blob_id in 
  (* Writes the file directly, hoping that the directories exist. *)
  begin 
    try Filesystem_store.write_gzipped_file temp_name s
    with Sys_error _ -> begin
      (* Makes the directories *)
      let make_dir (d: string) = 
	begin 
	  try Unix.mkdir d 0o755
	  with Unix.Unix_error (Unix.EEXIST, _, _) -> () 
	end
      in List.iter make_dir dir_l;
      (* Tries again to write the blob *)
      Filesystem_store.write_gzipped_file temp_name s
    end
  end;
  (* Now renames the blob to the final place, which is an atomic operation. *)
  Unix.rename temp_name f_name

(** [read_blob base_path page_id blob_id] returns the blob
    [blob_id] of page [page_id]. *)
let read_blob (base_path: string) (page_id: int) (blob_id: int) 
    : string option =
  let (f_name, _, _, _) = get_filename base_path page_id blob_id in 
  Filesystem_store.read_gzipped_file f_name;;

(** [delete_blob base_path page_id blob_id] deletes the blob
    [blob_id] of page [page_id]. *)
let delete_blob (base_path: string) (page_id: int) (blob_id: int) =
  let (f_name, _, _, _) = get_filename base_path page_id blob_id in 
  try
    Unix.unlink f_name
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(** [delete_all base_path] deletes all the tree of blobs rooted
    at base_path. *)
let delete_all (base_path: string) : Unix.process_status =
  Unix.system ("rm -rf " ^ base_path)


(* **************************************************************** *)
(* Write and read a revision from an (uncompressed) blob. *)

(** [assemble_blob [revisions] assembles the blob from the list of
    revisions [revisions].  Each element of [revision] is a pair 
    (revision_id, revision_text).  *)
let assemble_blob (revisions: (int * string) list) : string =
  (* This function, folded over [revisions], produces the header. *)
  let make_header (header_so_far, len_so_far) (r_id, r_txt) 
      : blob_header_t * int = 
    let r_len = String.length r_txt in 
    ((r_id, len_so_far, r_len) :: header_so_far, len_so_far + r_len)
  in 
  let (header, bin_len) = List.fold_left make_header ([], 0) revisions in
  let header_string = string_of__of__sexp_of sexp_of_blob_header_t header in
  (* Produces the buffer that will hold the blob. *)
  let l = (String.length header_string) + bin_len + 2 in
  let buf = Buffer.create l in
  Buffer.add_string buf header_string;
  Buffer.add_char buf separator_char;
  (* This function, iterated over [revisions], adds the revision content to
     the buffer. *)
  let add_rev_content (_, rev_txt) = Buffer.add_string buf rev_txt in
  List.iter add_rev_content revisions;
  (* The result is in the buffer. *)
  Buffer.contents buf


(** [disassemble_blob blob_content] takes apart a blob into the
    revisions that compose it, returning a list of 
    (revision_id, revision_len). *)
let disassemble_blob (blob_content: string) : (int * string) list =
  (* First, separates the header. *)
  let i = try String.index blob_content separator_char
  with Not_found -> raise Invalid_blob_format
  in 
  let header_string = String.sub blob_content 0 i in 
  let header = of_string__of__of_sexp blob_header_t_of_sexp header_string in
  let bin_offset = i + 1 in
  (* This function, folded over the header, produces the disassembled blob. *)
  let take_apart_blob rev_list (r_id, r_offset, r_len) = 
    let r_txt = String.sub blob_content (bin_offset + r_offset) r_len in
    (r_id, r_txt) :: rev_list
  in 
  List.fold_left take_apart_blob [] header


(** [add_revision_to_blob blob_content_opt rev_id markup] 
    adds the revision with content [markup] and revision id [rev_id]
    to the blob with optional content [blob_content_opt]. 
    The function returns the (non-optional) new blob, 
    along with the number of revisions it contains.  The latter
    is useful to avoid over-filling blobs. *)
let add_revision_to_blob (blob_content_opt: string option)
    (rev_id: int) (markup: string) : string * int = 
  match blob_content_opt with
    None -> (assemble_blob [(rev_id, markup)], 1)
  | Some blob_content -> begin
      (* Adds/replaces the content *)
      let rev_l = disassemble_blob blob_content in
      let rev_l_wo_id = List.remove_assoc rev_id rev_l in
      let new_rev_l = (rev_id, markup) :: rev_l_wo_id in
      (assemble_blob new_rev_l, List.length new_rev_l)
    end


(** [read_revision_from_blob rev_id blob_content] reads the 
    revision [rev_id] from the blob [blob_content], returning
    the revision text. *)
let read_revision_from_blob (rev_id: int) (blob_content: string) : string =
  let rev_l = disassemble_blob blob_content in
  List.assoc rev_id rev_l


(* **************************************************************** *)
(* Compression, decompression for db use *)

(** [compress s] compresses the string [s]. *)
let compress (s: string) : string = 
  let file_name = Filename.temp_file compress_prefix "_temp" in
  Filesystem_store.write_gzipped_file file_name s;
  let f = open_in file_name in
  (* Read the whole file. *)
  let buf = Buffer.create 100000 in
  let str_len = 8192 in 
  let str = String.create str_len in
  let read_more = ref true in
  while !read_more do begin
    let n_read = input f str 0 str_len in
    if n_read = 0 then read_more := false;
    Buffer.add_string buf (String.sub str 0 n_read)
  end done;
  close_in f;
  Unix.unlink file_name;
  Buffer.contents buf


(** [uncompress s] uncompresses the string [s]. *)
let uncompress (s: string) : string = 
  let file_name = Filename.temp_file compress_prefix "_temp" in
  let f = open_out file_name in
  output_string f s;
  close_out f;
  let r = match Filesystem_store.read_gzipped_file file_name with 
      None -> raise Compression_error
    | Some str -> str
  in 
  Unix.unlink file_name;
  r


(* **************************************************************** *)
(* Unit tests. *)

if false then begin
  let not_null = function
      None -> "Error: revision not found."
    | Some x -> x
  in 

  write_blob "/tmp/alpha" 43 54 "Ecco il mio blob";
  print_string (not_null (read_blob "/tmp/alpha" 43 54));

  print_string "\nAssemble then disassemble:\n";
  let b = assemble_blob [(1, "Revision n. 1"); (2, "Revision 2")] in
  let print_rev_str (i, s) = Printf.printf "%d: %S\n" i s in
  let print_blob x = List.iter print_rev_str (disassemble_blob x) in
  print_blob b;

  print_string "\nAdd revision then disassemble:\n";
  let (d, _) = add_revision_to_blob (Some b) 3 "Revision number 3" in
  print_blob d;
  print_string "\nAdd revision to empty blob:\n";
  let (e, f) = add_revision_to_blob None 3 "Lonely revision" in
  print_blob e; print_string " ; "; print_int f; 
  print_string "\nExtracting and writing revisions:\n";
  Printf.printf "%S " (read_revision_from_blob 2 b);
  Printf.printf "%S " (read_revision_from_blob 3 d);
  Printf.printf "%S " (read_revision_from_blob 1 d);

  print_string "\nCompress and uncompress:\n";
  print_string (uncompress (compress "Mi piace la pizza\n"));

end
