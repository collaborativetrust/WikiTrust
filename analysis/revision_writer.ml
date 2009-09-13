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

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"

(* **************************************************************** *)
(* Writer class to be used for batch writing. *)

open Online_types
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib
open Revision_store

(* Neither a db, not a filesystem prefix, has been specified. *)
exception No_write_method

type method_t = Disk_write of string | Database_write of Online_db.db

(* General read-write functions for all access methods. *)

let write_general_blob (write_method: method_t) (page_id: int) (blob_id: int) 
    (revisions: (int * string) list) : unit =
  let s = Revision_store.assemble_blob revisions in
  match write_method with
    Disk_write base_path -> 
      Revision_store.write_blob base_path page_id blob_id s
  | Database_write db -> db#write_blob page_id blob_id s

let read_general_blob (write_method: method_t) (page_id: int) (blob_id: int)
    : (int * string) list =
  let s =
    match write_method with
      Disk_write base_path ->
	Revision_store.read_blob base_path page_id blob_id
    | Database_write db -> db#read_blob page_id blob_id
  in match s with 
    None -> []
  | Some s' -> Revision_store.disassemble_blob s'


(** Writer class for consecutive writes. 
    We write to the db if a db is specified, and it has no base path.
    In all other cases, we write to the filesystem directly. *)

class writer 
  (page_id: int) 
  (first_blob_id_opt: int option)
  (base_path_opt: string option)
  (db_opt: Online_db.db option)
  = 

  object(self)

    val mutable write_method : method_t = Disk_write ""
    val mutable blob_id : int = blob_locations.invalid_location
    val mutable blob_revisions : (int * string) list = []
    val mutable blob_size = 0
    val max_size_blob = 10000000
    val max_n_revisions = 20
      
    initializer begin
      (* First, we have to establish whether we are writing to the 
         db or to a base path itself. *)
      begin
	match base_path_opt with
	  (* If defined, we know we don't use the db. *)
	  Some b -> write_method <- Disk_write b
	| None -> begin
	    match db_opt with 
	      None -> raise No_write_method
	    | Some db -> begin 
		match db#get_base_path with
		  Some b -> write_method <- Disk_write b
		| None -> write_method <- Database_write db
	      end
	  end
      end;
      (* Next, we have to initialize the blob_id, and, in case we are
	 not writing a page from scratch, we need to read the current
	 open blob from disk. *)
      begin
	match first_blob_id_opt with 
	  None -> blob_id <- blob_locations.initial_location
	| Some id -> begin
	    blob_id <- id;
	    (* We need to read that blob, and put it into blob_revisions, 
	       and if that blob is empty, we need to read the previous one
	       as well, as we need to be sure that in case of a vote, we
	       over-write rather than add any new revision. *)
	    begin
	      match read_general_blob write_method page_id blob_id with
		  (* We need to read the previous blob as well *)
		[] -> begin 
		  blob_id <- blob_id - 1;
		  blob_revisions <- 
		    read_general_blob write_method page_id blob_id
		end
	      | l -> blob_revisions <- l
	    end;
	    let f (r_id, r_txt) = 
	      blob_size <- blob_size + String.length r_txt
	    in List.iter f blob_revisions
	  end
      end
    end (* initializer *)


    (** This method queues a revision for writing, and returns the
	blob id where the revision will be written.
	We admit the case where the same revision is written multiple 
	times in a row, as it happens when votes are processed; in this case,
	only one version of the revision (the latest) is written to disk. *)
    method write_revision (rev_id: int) (rev_txt: string) : int =
      (* Checks whether the revision should replace the last one *)
      match blob_revisions with
	(rev_id, old_txt) :: previous_revisions -> begin
	  (* The revision is being written multiple times; we replace the
	     older content. *)
	  blob_revisions <- (rev_id, rev_txt) :: previous_revisions;
	  blob_size <- 
	    blob_size - (String.length old_txt) + (String.length rev_txt);
	  blob_id
	end
      | _ -> begin
	  (* The revision id is different from the previous one. *)
	  (* First, checks whether the older revisions need writing to disk. *)
	  let n_revisions = List.length blob_revisions in 
	  if blob_size >= max_size_blob || n_revisions >= max_n_revisions
	  then begin
	    (* Writes to disk. *)
	    write_general_blob write_method page_id blob_id blob_revisions;
	    blob_id <- blob_id + 1;
	    blob_revisions <- [];
	  blob_size <- 0
	  end;
	  (* Appends the new revision. *)
	  blob_revisions <- (rev_id, rev_txt) :: blob_revisions;
	  blob_size <- blob_size + (String.length rev_txt);
	  blob_id
	end
	  
	  
    (** This method finishes writing any pending revision,
	and returns the last blob used (which is the open blob). *)
    method close : int =
      match blob_revisions with 
	[] -> blob_id;
      | _ :: _ -> begin
	  (* Writes the current blob. *)
	  write_general_blob write_method page_id blob_id blob_revisions;
	  (* Checks whether we need to move to a new blob. *)
	  let n_revisions = List.length blob_revisions in
	  if blob_size >= max_size_blob || n_revisions >= max_n_revisions
	  then blob_id + 1
	  else blob_id
	end
	  
  end (* class writer *)

(* **************************************************************** *)
(* Unit tests. *)

if false then begin
  let not_null = function
      None -> "Error: revision not found."
    | Some x -> x
  in 

  let print_rev_str (i, s) = Printf.printf "%d: %S\n" i s in
  let print_blob x = List.iter print_rev_str (disassemble_blob x) in

  print_string "\nTest for the writer:\n";
  let w = new writer 12 None (Some "/tmp/alpha") None in
  print_string "First blob written as blob id "; 
  print_int (w#write_revision 43 "hello hello");
  let blob_id = w#write_revision 47 "ciao ciao" in
  print_string "\nSecond blob written as blob id ";
  print_int blob_id; 
  ignore w#close;
  print_string "\nReading the blob:\n";
  print_blob (not_null (read_blob "/tmp/alpha" 12 blob_id))
end
