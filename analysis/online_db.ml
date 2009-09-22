(*

Copyright (c) 2008-09 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye, B. Thomas Adler

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


open Online_types
open Mysql
open Sexplib.Conv
open Sexplib.Sexp
open Sexplib
open Online_log


TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"

(* Returned whenever something is not found *)
exception DB_Not_Found
(* Internal error *)
exception DB_Internal_Error
(* Commit failed, or other database error that may have to cause a rollback. *)
exception DB_TXN_Bad
(* ExecAPI failed *)
exception DB_Exec_Error
(* Read/write colored revisions from wrong blob ids. *)
exception DB_Illegal_blob_id

(* This is the function that sexplib uses to convert floats *)
Sexplib.Conv.default_string_of_float := (fun n -> Printf.sprintf "%.3f" n);;

(* Represents the revision table in memory *)
type revision_t = {
  rev_id: int; 
  rev_page: int; 
  rev_text_id: int; 
  rev_timestamp: string; 
  rev_user: int; 
  rev_user_text: string; 
  rev_is_minor: bool;
} 

let set_is_minor ism = match ism with
  | 0 -> false
  | 1 -> true
  | _ -> assert false 

(* Given a row, return a new online revision *)
let rev_row2revision_t row =
  {
    rev_id = (not_null int2ml row.(0)); (* rev id *)
    rev_page = (not_null int2ml row.(1)); (* page id *)        
    rev_text_id = (not_null int2ml row.(2)); (* text id *)
    rev_timestamp = (not_null str2ml row.(3)); (* timestamp *)
    rev_user = (not_null int2ml row.(4)); (* user id *)
    rev_user_text = (not_null str2ml row.(5)); (* user name *)
    rev_is_minor = (set_is_minor (not_null int2ml row.(6))); (* is_minor *)
  } 

(* This is the type of a vote data *)
type vote_t = {
  vote_time: string;
  vote_page_id: int; 
  vote_revision_id: int;
  vote_voter_name: string;
}

(* This is the type of a set of signatures, as visible from outside. *)
type page_sig_t = page_sig_disk_t
let empty_page_sigs = []

(* Produces the key for a signature *)
let make_blob_key (page_id: int) (blob_id: int) : string =
  Printf.sprintf "%012d%012d" page_id blob_id

(** [get_cmd_output cmdline] returns the output from an external command.
    An exception is thrown if there is a problem.  *)
let get_cmd_output (cmdline: string) : string =
  let cmdout = Unix.open_process_in cmdline in 
  let bufSize = 8192 in
  let buf = String.create bufSize in
  let completeFile = Buffer.create bufSize in
  let rec loop () =
    let chunkLen = input cmdout buf 0 bufSize in
    if (chunkLen > 0) then begin
      let actualChunk = String.sub buf 0 chunkLen in
      Buffer.add_string completeFile actualChunk;
      loop ()
    end else
      raise End_of_file
  in begin
    try
      loop ()
    with End_of_file -> begin
      let status = Unix.close_process_in cmdout in
      match status with
        | Unix.WEXITED 0 -> Buffer.contents completeFile
        | _ -> raise DB_Exec_Error
    end
  end


(** This class provides a handle for accessing the database in the on-line 
    implementation. *)

class db  
  (db_prefix : string)
  (mediawiki_dbh : Mysql.dbd)
  (db_name: string)
  (rev_base_path: string option)
  (colored_base_path: string option)
  (max_size_per_blob: int)
  (max_revs_per_blob: int)
  (debug_mode : bool) =
  
object(self)

  method private db_exec dbh s = 
    if debug_mode then begin 
      print_endline s;
      flush stdout
    end;
    try 
      Mysql.exec dbh s
    with Mysql.Error e -> if debug_mode then 
      begin print_endline e; flush stdout end; 
      raise DB_TXN_Bad

  (* ================================================================ *)
  (* General methods *)
  method get_base_path : string option = colored_base_path

  (* ================================================================ *)
  (* Disconnect *)
  method close : unit = 
    Mysql.disconnect mediawiki_dbh


  (* ================================================================ *)
  (* Locks and commits. *)

  (** [get_page_lock page_id timeout] gets a lock for page [page_id],
      to guarantee mutual exclusion on the updates for page [page_id].
      The lock is waited for at most time [timeout] seconds.  The
      function returns [true] if the lock was acquired. *)
  method get_page_lock (page_id: int) (timeout: int) : bool = 
    let lock_name = Printf.sprintf "%s.%swikitrust_page.%d" db_name db_prefix page_id in 
    let s = Printf.sprintf "SELECT GET_LOCK(%s,%s)" (ml2str lock_name) (ml2int timeout) in 
    match fetch (self#db_exec mediawiki_dbh s) with 
      None -> raise DB_Internal_Error
    | Some row -> ((not_null int2ml row.(0)) = 1)

  (** [is_page_lock_free page_id] checks whether the lock for page
      [page_id] is available (there is no guarantee that somebody does
      not lock the page between this test and a subsequent call to
      [get_page_lock]. *)
  method is_page_lock_free (page_id: int) : bool = 
    let lock_name = Printf.sprintf "%s.%swikitrust_page.%d" db_name db_prefix page_id in 
    let s = Printf.sprintf "SELECT IS_FREE_LOCK(%s)" (ml2str lock_name) in 
    match fetch (self#db_exec mediawiki_dbh s) with 
      None -> raise DB_Internal_Error
    | Some row -> ((not_null int2ml row.(0)) = 1)

  (** [release_page_lock page_id] releases the lock for page
      [page_id], to guarantee mutual exclusion on the updates for page
      [page_id]. *)
  method release_page_lock (page_id: int) : unit = 
    let lock_name = Printf.sprintf "%s.%swikitrust_page.%d" db_name db_prefix page_id in 
    let s = Printf.sprintf "SELECT RELEASE_LOCK(%s)" (ml2str lock_name) in 
    ignore (self#db_exec mediawiki_dbh s)

  (** Start a transaction. *)
  method start_transaction : unit =
    ignore (self#db_exec mediawiki_dbh "START TRANSACTION")

  (** rollback a transaction. *)
  method rollback_transaction : unit =
    !online_logger#log "ROLLBACK\n";
    ignore (self#db_exec mediawiki_dbh "ROLLBACK")
      
  (* Commits any changes to the db *)
  method commit : unit =
    ignore (self#db_exec mediawiki_dbh "COMMIT");
    match Mysql.status mediawiki_dbh with 
    | StatusError err -> raise DB_TXN_Bad
    | _ -> ()
	
	
  (* ================================================================ *)
  (* Global methods. *)
	
  (** [get_histogram] Returns a histogram showing the number of users 
      at each reputation level, and the median. *)
  method get_histogram : float array * float =
    let s = Printf.sprintf "SELECT * FROM %swikitrust_global" db_prefix in
    match fetch (self#db_exec mediawiki_dbh s) with
      None -> begin
	(* No histogram is found.  It creates a default one, and returns it. *)
	let s = Printf.sprintf "INSERT INTO %swikitrust_global VALUES (0,0,0,0,0,0,0,0,0,0,0)" db_prefix in
	ignore (self#db_exec mediawiki_dbh s);
	([|0.; 0.; 0.;  0.; 0.; 0.;  0.; 0.; 0.;  0.|],  0.)
      end
    | Some row -> ([| not_null float2ml row.(1); not_null float2ml row.(2); not_null float2ml row.(3); 
      not_null float2ml row.(4);  not_null float2ml row.(5);
      not_null float2ml row.(6);  not_null float2ml row.(7);
      not_null float2ml row.(8);  not_null float2ml row.(9); not_null float2ml row.(10);
      |], (not_null float2ml row.(0)))
	
  (** write_histogram delta_hist median] increments the db histogram
      of user reputations according to the array [delta_hist], and
      writes that the new median is [median]. *)
  method write_histogram (delta_hist : float array) (median: float) : unit = 
    let s = Printf.sprintf  "UPDATE %swikitrust_global SET median = %s, rep_0 = rep_0 + %s, rep_1 = rep_1 + %s, rep_2 = rep_2 + %s, rep_3 = rep_3 + %s, rep_4 = rep_4 + %s, rep_5 = rep_5 + %s, rep_6 = rep_6 + %s, rep_7 = rep_7 + %s, rep_8 = rep_8 + %s, rep_9 = rep_9 + %s" 
      db_prefix
      (ml2float median)
      (ml2float delta_hist.(0)) (ml2float delta_hist.(1)) (ml2float delta_hist.(2)) (ml2float delta_hist.(3)) 
      (ml2float delta_hist.(4)) (ml2float delta_hist.(5)) (ml2float delta_hist.(6)) (ml2float delta_hist.(7)) 
      (ml2float delta_hist.(8)) (ml2float delta_hist.(9)) in 
    ignore (self#db_exec mediawiki_dbh s)


  (** [fetch_last_colored_rev_time req_page_id] returns the timestamp and the 
      revision id of the most recent revision that has been colored.
      If [req_page_id] specifies a page, then only that page is considered.
      Raises DB_Not_Found if no revisions have been colored. *)
      (* FIX: look at al the LIMIT 1 and see if you can use max instead. *)
  method fetch_last_colored_rev_time (req_page_id: int option) : (string * int) = 
    let wr = match req_page_id with
	None -> ""
      | Some p_id ->  Printf.sprintf "WHERE page_id = %s" (ml2int p_id) 
    in 
    let s = Printf.sprintf "SELECT time_string, revision_id FROM %swikitrust_revision %s ORDER BY time_string DESC, revision_id DESC LIMIT 1" db_prefix wr in
    match fetch (self#db_exec mediawiki_dbh s) with
      None -> raise DB_Not_Found
    | Some row -> (not_null str2ml row.(0), not_null int2ml row.(1)) 
	
	
  (** [fetch_all_revs_after req_page_id req_rev_id timestamp rev_id
      limit] returns all revs created after the given [timestamp], or
      at the same [timestamp], with revision id at least [rev_id], up
      to the maximim number [limit].  If [req_page_id] specifies a
      page, then only that page is considered.  If [req_rev_id]
      specifies a revision, then that revision is included in the
      result. *)
  method fetch_all_revs_after
    (req_page_id: int option) 
    (req_rev_id: int option) 
    (timestamp : string) 
    (rev_id : int) 
    (max_revs_to_return : int)
    : revision_t list =  
    let wr = match req_page_id with
      | None -> ""
      | Some p_id -> Printf.sprintf "AND rev_page = %s" (ml2int p_id) 
    in
    let rr = match req_rev_id with
      | None -> ""
      | Some r_id -> Printf.sprintf "rev_id = %s OR" (ml2int r_id)
    in
    let s = Printf. sprintf "SELECT rev_id, rev_page, rev_text_id, rev_timestamp, rev_user, rev_user_text, rev_minor_edit FROM %srevision WHERE %s (rev_timestamp, rev_id) > (%s, %s) %s ORDER BY rev_timestamp ASC, rev_id ASC LIMIT %s" 
      db_prefix
      rr (ml2str timestamp) (ml2int rev_id) wr (ml2int max_revs_to_return) in
    Mysql.map (self#db_exec mediawiki_dbh s) rev_row2revision_t
      

  (** [fetch_all_revs req_page_id max_revs_to_return] returns a list
      of revisions in the database, in ascending order of timestamp,
      of length at most [max_revs_to_return].  If [req_page_id]
      specifies a page, then only that page is considered. *)
  method fetch_all_revs (req_page_id: int option) (max_revs_to_return: int) : revision_t list = 
    let wr = match req_page_id with
	None -> ""
      | Some p_id -> Printf.sprintf "WHERE rev_page = %s" (ml2int p_id) 
    in
    let s = Printf.sprintf  "SELECT rev_id, rev_page, rev_text_id, rev_timestamp, rev_user, rev_user_text, rev_minor_edit FROM %srevision %s ORDER BY rev_timestamp ASC, rev_id ASC LIMIT %s" 
      db_prefix wr (ml2int max_revs_to_return) in
    Mysql.map (self#db_exec mediawiki_dbh s) rev_row2revision_t


  (** [fetch_unprocessed_votes req_page_id n_events] returns at most
      [n_events] unprocessed votes, starting from the oldest
      unprocessed vote.  If [req_page_id] specifies a page, then only
      that page is considered. *)
  method fetch_unprocessed_votes (req_page_id: int option) (n_events: int) : vote_t list = 
    let wr = match req_page_id with
	None -> ""
      | Some p_id -> Printf.sprintf "page_id = %s AND" (ml2int p_id) 
    in
    let s = Printf.sprintf  "SELECT voted_on, page_id, revision_id, voter_name FROM %swikitrust_vote WHERE %s NOT processed ORDER BY voted_on ASC LIMIT %s" db_prefix wr (ml2int n_events) in
    let vote_row2vote_t row =
      {
	vote_time = (not_null str2ml row.(0));
	vote_page_id = (not_null int2ml row.(1));
	vote_revision_id = (not_null int2ml row.(2));
	vote_voter_name = (not_null str2ml row.(3));
      }
    in
    Mysql.map (self#db_exec mediawiki_dbh s) vote_row2vote_t


  (** [mark_vote_as_processed (revision_id: int) (voter_name :
      string)] marks a vote as processed. *)
  method mark_vote_as_processed (revision_id: int) (voter_name : string) 
    : unit = 
    let s = Printf.sprintf "UPDATE %swikitrust_vote SET processed = TRUE WHERE revision_id = %s AND voter_name = %s" 
      db_prefix (ml2int revision_id) (ml2str voter_name) in
    ignore (self#db_exec mediawiki_dbh s)

  (* ================================================================ *)
  (* Blob methods. *)
   
  (** [read_blob page_id blob_id] reads the blob for page_id and blob_id,
      either from the database, or from the filesystem, and returns it. *)
  method read_blob (page_id: int) (blob_id: int) 
    : string option =
    match colored_base_path with
      None -> begin
	(* The blobs are stored in the db. *)
	let key_str = make_blob_key page_id blob_id in
	let s = Printf.sprintf "SELECT blob_content from %swikitrust_blob WHERE blob_id = %s" db_prefix (ml2decimal key_str) in
	let result = self#db_exec mediawiki_dbh s in 
	match Mysql.fetch result with 
	  None -> None
	| Some x -> Some (Revision_store.uncompress (not_null blob2ml x.(0)))
      end
    | Some b -> Revision_store.read_blob b page_id blob_id


  (** [write_blob page_id blob_id blob_content] writes the blob [blob_content]
      for [page_id], [blob_id] to either the filesystem or the database. *)
  method write_blob 
    (page_id: int) (blob_id: int) (blob_content: string) : unit =
    match colored_base_path with
      None -> begin
	(* The blobs are stored in the db. *)
	let key_str = make_blob_key page_id blob_id in
	let compressed_blob = Revision_store.compress blob_content in
	let blob_db = ml2blob compressed_blob in
	let s = Printf.sprintf "DELETE FROM %swikitrust_blob WHERE blob_id = %s"
	  db_prefix (ml2decimal key_str) in
	ignore (self#db_exec mediawiki_dbh s);
	let s = Printf.sprintf "INSERT INTO %swikitrust_blob (blob_id, blob_content) VALUES (%s, %s)" db_prefix (ml2decimal key_str) blob_db in
	ignore (self#db_exec mediawiki_dbh s)
      end 
    | Some b -> Revision_store.write_blob b page_id blob_id blob_content


  (* ================================================================ *)
  (* Page methods. *)

  (* Methods for wikitrust_page *)

  (** [init_page page_id] initializes the page information for 
      page [page_id]. *)
  method init_page (page_id : int) (page_title : string option) : unit =
    let info_string = ml2str 
      (string_of__of__sexp_of sexp_of_page_info_t 
	Online_types.page_info_default) in 
    let s = Printf.sprintf "INSERT INTO %swikitrust_page (page_id, page_info, page_title, last_blob) VALUES (%s, %s, %s, %s) ON DUPLICATE KEY UPDATE last_blob = last_blob" 
      db_prefix (ml2int page_id) info_string (match page_title with 
                                                | Some pt -> ml2str pt 
                                                | None -> "null")
      (ml2int blob_locations.initial_location) in
    ignore (self#db_exec mediawiki_dbh s)    
    
	
  (** [write_page_info page_id p_info] writes that the page [page_id]
      has associated information [p_info].  The list of deleted chunks
      of the page is not modified. *)
  method write_page_info (page_id : int) (p_info: page_info_t) : unit = 
    let info_string = ml2str 
      (string_of__of__sexp_of sexp_of_page_info_t p_info) in 
    let s = Printf.sprintf "UPDATE %swikitrust_page SET page_info = %s WHERE page_id = %s"
      db_prefix info_string (ml2int page_id) in 
    ignore (self#db_exec mediawiki_dbh s)

  (** [read_page_info page_id] returns the page information for [page_id],
      consisting of the page information proper and the open_page_blob_id. *)
  method read_page_info (page_id : int) : page_info_t * int =
    let s = Printf.sprintf "SELECT page_info, last_blob FROM %swikitrust_page WHERE page_id = %s" db_prefix (ml2int page_id) in
    let result = self#db_exec mediawiki_dbh s in 
    match Mysql.fetch result with 
      None -> raise DB_Not_Found
    | Some x -> 
	(of_string__of__of_sexp page_info_t_of_sexp (not_null str2ml x.(0)),
	not_null int2ml x.(1))

  (** [fetch_col_revs page_id timestamp rev_id fetch_limit] returns
      a cursor that points to at most [fetch_limit] revisions of
      page [page_id] with time prior or equal to [timestamp], and
      revision id at most [rev_id].  The ordering goes backward in time. 
      This function is used to read the colored revisions that precede a 
      given one. *)
  method fetch_col_revs (page_id : int) (timestamp: timestamp_t) (rev_id: int) (fetch_limit: int): Mysql.result =
    let s = Printf.sprintf "SELECT revision_id, page_id, text_id, time_string, user_id, username, is_minor FROM %swikitrust_revision WHERE page_id = %s AND (time_string, revision_id) < (%s, %s) ORDER BY time_string DESC, revision_id DESC LIMIT %s" db_prefix (ml2int page_id) (ml2timestamp timestamp) (ml2int rev_id) (ml2int fetch_limit) in 
    self#db_exec mediawiki_dbh s

  (* Chunk methods *)

  (** [write_page_chunks page_id chunk_list] writes that the page
      with id [page_id] is associated with the "dead" strings of text
      [chunk1], [chunk2], ..., where [chunk_list = [chunk1, chunk2,
      ...] ].  The chunk_list contains text that used to be present in
      the article, but has been deleted.  *)
  method write_page_chunks (page_id : int) (c_list : chunk_t list) : unit = 
    let chunks_string = 
      (string_of__of__sexp_of (sexp_of_list sexp_of_chunk_t) c_list) in 
    self#write_blob page_id blob_locations.chunks_location chunks_string

  (** [read_page_chunks page_id] returns the chunk list for page [page_id]. 
      If the chunks cannot be found, returns the empty list: this happens
      for new pages. *)
  method read_page_chunks (page_id: int) : chunk_t list =
    let chunks_string = self#read_blob page_id blob_locations.chunks_location in
    match chunks_string with
      None -> []
    | Some s -> of_string__of__of_sexp (list_of_sexp chunk_t_of_sexp) s
    
  (** [write_open_blob_id page_id blob_id] writes on the wikitrust_page table that
      the open blob for [page_id] is [blob_id]. *)
  method write_open_blob_id (page_id: int) (blob_id: int) : unit =
    let s = Printf.sprintf "UPDATE %swikitrust_page SET last_blob = %s WHERE page_id = %s" db_prefix (ml2int blob_id) (ml2int page_id) in
    ignore (self#db_exec mediawiki_dbh s)

  (* Signature methods *)

  (** [read_page_sigs page_id] reads and returns the sigs for page
      [page_id]. *)
  method read_page_sigs (page_id: int) : page_sig_t =
    let s = self#read_blob page_id blob_locations.sig_location in
    match s with
      None -> []
    | Some s' -> (of_string__of__of_sexp page_sig_disk_t_of_sexp s')

  (** [write_page_sigs page_id sigs] writes that the sigs 
      page [page_id] are [sigs]. *)
  method write_page_sigs (page_id: int) (sigs: page_sig_t) : unit =
    (* Creates a single string for the sigs. *)
    let sig_string = string_of__of__sexp_of sexp_of_page_sig_disk_t sigs in
    self#write_blob page_id blob_locations.sig_location sig_string

  (* Methods on the standard tables *)

  (** [get_latest_rev_id page_title] returns the revision id of the most 
      recent revision of page [page_title], according. *)
  method get_latest_rev_id (page_title: string) : int = 
    let s = Printf.sprintf "SELECT revision_id FROM %swikitrust_revision AS A JOIN %swikitrust_page AS B on A.page_id = B.page_id AND page_title = %s ORDER BY time_string DESC, revision_id DESC LIMIT 1" db_prefix db_prefix (ml2str page_title) in 
    match fetch (self#db_exec mediawiki_dbh s) with 
      None -> raise DB_Not_Found
    | Some x -> not_null int2ml x.(0)

  (** [get_page_id page_title] returns the page id of the named page *)
  method get_page_id (page_title: string) : int = 
    let s = Printf.sprintf "SELECT page_id FROM %swikitrust_page WHERE page_title = %s LIMIT 1" db_prefix (ml2str page_title) in 
    match fetch (self#db_exec mediawiki_dbh s) with 
      None -> raise DB_Not_Found
    | Some x -> not_null int2ml x.(0)

  (** [get_page_title page_id] returns the page title of the named page *)
  method get_page_title (page_id: int) : string = 
    let s = Printf.sprintf "SELECT page_title FROM %swikitrust_page WHERE page_id = %d LIMIT 1" db_prefix page_id in 
    match fetch (self#db_exec mediawiki_dbh s) with 
      None -> raise DB_Not_Found
    | Some x -> not_null str2ml x.(0)


  (* ================================================================ *)
  (* Revision methods. *) 

  (** [read_wikitrust_revision rev_id] reads a revision from the 
      wikitrust_revision table, returning the revision information,
      the quality information, and the optional blob id. *)
  method read_wikitrust_revision (revision_id: int) 
    : (revision_t * qual_info_t * int option) = 
    let s = Printf.sprintf "SELECT revision_id, page_id, text_id, time_string, user_id, username, is_minor, quality_info, blob_id FROM %swikitrust_revision WHERE revision_id = %s" db_prefix (ml2int revision_id) in 
    let result = self#db_exec mediawiki_dbh s in 
    match fetch result with 
      None -> raise DB_Not_Found
    | Some x -> begin 
	let r = {
	  rev_id = not_null int2ml x.(0); 
	  rev_page = not_null int2ml x.(1); 
	  rev_text_id = not_null int2ml x.(2); 
	  rev_timestamp = not_null str2ml x.(3); 
	  rev_user = not_null int2ml x.(4); 
	  rev_user_text = not_null str2ml x.(5); 
	  rev_is_minor = set_is_minor (not_null int2ml x.(6)); 
	} in 
	let q = of_string__of__of_sexp qual_info_t_of_sexp 
	  (not_null str2ml x.(8)) in 
	let bid = not_null int2ml x.(9) in
	let bid_opt = 
	  if bid = blob_locations.invalid_location
	  then None
	  else Some bid
	in (r, q, bid_opt)
      end

  (** [write_wikitrust_revision revision_info quality_info]
      writes the wikitrust data associated with a revision. *)
  method write_wikitrust_revision 
    (revision_info: revision_t) 
    (quality_info: qual_info_t) 
    (blob_id_opt: int option) : unit = 
    (* Blob id *)
    let blob_id = match blob_id_opt with
	Some k -> k
      | None -> blob_locations.invalid_location
    in
    let blob_id_db = ml2int blob_id in
    (* Revision parameters *)
    let rev_id = ml2int revision_info.rev_id in
    let page_id = ml2int revision_info.rev_page in 
    let text_id = ml2int revision_info.rev_text_id in 
    let time_string = ml2str revision_info.rev_timestamp in 
    let user_id = ml2int revision_info.rev_user in 
    let username = ml2str revision_info.rev_user_text in 
    let is_minor = ml2int (if revision_info.rev_is_minor then 1 else 0) in 
    let comment = ml2str "" in
    (* Quality parameters *)
    let q1 = ml2str (string_of__of__sexp_of sexp_of_qual_info_t quality_info) in
    let q2 =  ml2float quality_info.reputation_gain in 
    let aq2 = if (q2 = "inf") then (ml2float infinity) else q2 in
    let q3 = ml2float quality_info.overall_trust in
    (* Db write access *)
    let s2 =  Printf.sprintf "INSERT INTO %swikitrust_revision (revision_id, page_id, text_id, time_string, user_id, username, is_minor, comment, quality_info, reputation_delta, overall_trust, blob_id) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) ON DUPLICATE KEY UPDATE quality_info = %s, reputation_delta = %s, overall_trust = %s, blob_id = %s"
      db_prefix rev_id page_id text_id time_string user_id username is_minor comment q1 aq2 q3 blob_id_db q1 aq2 q3 blob_id_db in
    ignore (self#db_exec mediawiki_dbh s2)


  (** [read_revision_info rev_id] reads the wikitrust information
      quality, and blob_id, of revision_id *)
  method read_revision_quality (rev_id: int) : qual_info_t * int option = 
    let s = Printf.sprintf "SELECT quality_info, blob_id FROM %swikitrust_revision WHERE revision_id = %s" db_prefix (ml2int rev_id) in 
    let result = self#db_exec mediawiki_dbh s in
    match fetch result with
      None -> raise DB_Not_Found
    | Some x -> begin
	let q = of_string__of__of_sexp qual_info_t_of_sexp 
	  (not_null str2ml x.(0)) in 
	let bid = not_null int2ml x.(1) in
	let bid_opt = 
	  if bid = blob_locations.invalid_location
	  then None
	  else Some bid
	in (q, bid_opt)
      end

  (** [write_colored_markup page_id rev_id blob_id_opt page_open_blob markup] 
      writes the "colored" text [markup] of a revision.  The [markup]
      represents the main text of the revision, annotated with trust
      and origin information. [page_id] and [rev_id] are as usual. 
      [blob_id_opt] specifies the blob in which the information should
      be written, if known.  Otherwise, the information is written in 
      [page_open_blob] blob.  The function returns a pair, consisting of:
      - The blob in which the revision was written 
        (this coincides with the content of [blob_id_opt] when the latter 
        is not null). 
      - The new open blob for the page (this may coincide with the old
        open blob, of course).
   *)
  method write_colored_markup (page_id: int) (rev_id : int) 
    (blob_id_opt: int option) (page_open_blob: int)
    (markup : string) : int * int =
    (* Figures out in which blob to write the information. *)
    let blob_id = 
      match blob_id_opt with 
	None -> page_open_blob
      | Some bid -> bid
    in
    (* Validates blob_id *)
    if blob_id < blob_locations.initial_location then 
      raise DB_Illegal_blob_id;
    (* Writes the information in the blob, and writes the blob back to disk. *)
    let blob_content_opt = self#read_blob page_id blob_id in
    let (new_blob_content, new_n_revs_in_blob) = 
      Revision_store.add_revision_to_blob blob_content_opt rev_id markup in
    self#write_blob page_id blob_id new_blob_content;
    (* If we added the revision to an open blob, and this open blob has
       become too large, then it opens a new blob for writing. *)
    let new_page_open_blob = match blob_id_opt with
	Some _ -> page_open_blob  (* No change: we wrote to an old blob. *)
      | None -> begin
	  (* We had to add to the open blob *)
	  let blob_size = String.length new_blob_content in
	  if blob_size > max_size_per_blob ||
	    new_n_revs_in_blob >= max_revs_per_blob then begin 
	      (* We start a new blob. *)
	      self#write_open_blob_id page_id (page_open_blob + 1);
	      page_open_blob + 1
	    end 
	  else page_open_blob
	end
    in (blob_id, new_page_open_blob)


  (** [read_colored_markup rev_id blob_id_opt] reads the text markup
      of a revision with id [rev_id].  The markup is the text of the
      revision, annotated with trust and origin information. 
      The revision can be found in [blob_id_opt] (if not None). 
      If the revision cannot be found, including if [blob_id_opt] is None, 
      the DB_Not_Found exception is raised. *)
  method read_colored_markup (page_id: int) (rev_id : int) 
    (blob_id_opt: int option) : string =
    let blob_id = begin match blob_id_opt with 
	None -> raise DB_Not_Found
      | Some bid -> bid
    end in 
    (* Validates the blob_id *)
    if blob_id < blob_locations.initial_location then 
      raise DB_Illegal_blob_id;
    let blob_content_opt = self#read_blob page_id blob_id in
    match blob_content_opt with
      None -> raise DB_Not_Found
    | Some c -> begin
	try Revision_store.read_revision_from_blob rev_id c
	with Not_found -> raise DB_Not_Found
      end


  (** [write_trust_origin_sigs page_id rev_id page_sigs words trust
      origin sigs] writes that the revision [rev_id] is associated
      with [words], [trust], [origin], and [author_sigs].  The
      function is given the page signatures [page_sigs], and does not
      actually write things to disk; rather, it updates these
      page_sigs in-place. *)
  method write_words_trust_origin_sigs (page_id: int) (rev_id: int) (page_sigs: page_sig_t)
    (words: string array)
    (trust: float array)
    (origin: int array)
    (author: string array)
    (author_sigs: Author_sig.packed_author_signature_t array) : page_sig_t =
    let signature = {
      words_a = words;
      trust_a = trust;
      origin_a = origin;
      author_a = author;
      sig_a = author_sigs;
    } in 
    (* If the revision already has sigs, removes them *)
    let l_purged = List.remove_assoc rev_id page_sigs in
    (* Adds the new signatures *)
    let l_added = (rev_id, signature) :: l_purged in
    (* Assigns the new value *)
    l_added

  (** [read_words_trust_origin_sigs page_id rev_id page_sigs] reads
      the words, trust, origin, and author sigs for the revision
      [rev_id] of page [page_id], given the page sigs [page_sigs]. *)
  method read_words_trust_origin_sigs (page_id: int) (rev_id: int) 
    (page_sigs: page_sig_t)
    : (string array * float array * int array * string array * Author_sig.packed_author_signature_t array) =
    if List.mem_assoc rev_id page_sigs
    then begin 
      let signature = List.assoc rev_id page_sigs in
      (signature.words_a, signature.trust_a, signature.origin_a, signature.author_a, signature.sig_a)
    end else raise DB_Not_Found

  (** [delete_author_sigs page_id rev_id page_sigs] removes from the
      signatures [page_sigs] the signatures for [rev_id]. *)
  method delete_author_sigs (page_id: int) (rev_id: int) (page_sigs: page_sig_t)
    : page_sig_t =
    List.remove_assoc rev_id page_sigs


  (* Methods on standard revisions *)

  (** [get_rev_text page_id rev_id text_id] returns the text associated with
      text id [text_id] for revision [rev_id] *)
  method read_rev_text (page_id: int) (rev_id: int) (text_id: int) : string =
    match rev_base_path with 
      None -> begin
	let s = Printf.sprintf "SELECT old_text FROM %stext WHERE old_id = %s" db_prefix (ml2int text_id) in
	let result = self#db_exec mediawiki_dbh s in 
	match Mysql.fetch result with 
          None -> raise DB_Not_Found
	| Some y -> not_null str2ml y.(0)
      end
    | Some b -> begin
	let result = Filesystem_store.read_revision b page_id rev_id in
	match result with 
	  None -> raise DB_Not_Found
	| Some r -> r
      end

  (* ================================================================ *)
  (* User methods. *)

  (** [inc_rep uid delta uname] increments the reputation of user [uid] by
      [delta] in a single operation, so to avoid database problems. 
      [uname] is the username of the user, to ensure that we know 
      the names of the users. *)
  method inc_rep (uid : int) (delta : float) (uname: string) =
    let s = Printf.sprintf "INSERT INTO %swikitrust_user (user_id, user_rep, username) VALUES (%s, %s, %s) ON DUPLICATE KEY UPDATE user_rep = user_rep + %s" 
      db_prefix
      (ml2int uid) (ml2float delta) (ml2str uname) (ml2float delta) in 
    ignore (self#db_exec mediawiki_dbh s)
      
  (** [get_rep uid] gets the reputation of user [uid], from a table 
      relating user ids to their reputation 
      @raise DB_Not_Found if no tuple is returned by the database.
   *)
  method get_rep (uid : int) : float =
    let s = Printf.sprintf "SELECT user_rep FROM %swikitrust_user WHERE user_id = %s" db_prefix (ml2int uid) in
    let result = self#db_exec mediawiki_dbh s in
    match Mysql.fetch result with 
      None -> raise DB_Not_Found
    | Some x -> not_null float2ml x.(0)


  (** [get_user_id name] gets the user id for the user with the given
      user name *)
  method get_user_id (user_name : string) : int =
    let s = Printf.sprintf "SELECT user_id FROM %swikitrust_user WHERE username = %s" db_prefix (ml2str user_name) in
    let result = self#db_exec mediawiki_dbh s in
    begin
      match Mysql.fetch result with 
        None -> raise DB_Not_Found
      | Some x -> not_null int2ml x.(0)
    end

  (** [write_user_id user_name] writes that the user with 
      name [user_name]. and returns the new uid *) 
  method write_user_id (user_name: string) : int = 
    let s = Printf.sprintf "INSERT INTO %swikitrust_user (username) VALUES (%s) ON DUPLICATE KEY UPDATE username = %s" db_prefix (ml2str user_name) (ml2str user_name) in
      ignore (self#db_exec mediawiki_dbh s);
      Int64.to_int (insert_id mediawiki_dbh)


  (* ================================================================ *)
  (* Votes. *)

  (** Add the vote to the db *)
  method vote (vote : vote_t) =
    let s = Printf.sprintf "INSERT INTO %swikitrust_vote (revision_id, page_id, voter_name, voted_on) VALUES (%s, %s, %s, %s)" db_prefix (ml2int vote.vote_revision_id) (ml2int vote.vote_page_id) (ml2str vote.vote_voter_name) (ml2str vote.vote_time) in
    ignore (self#db_exec mediawiki_dbh s)


  (* ================================================================ *)
  (* Server System *)
  (* The following methods are used in the remote use of WikiTrust. *)

  (** [mark_page_to_process page_id page_title] specifies that a page
      must be brought up to date, due to a vote or a new revision. *)
  method mark_page_to_process (page_id : int) (page_title : string) : unit =
    let s = Printf.sprintf "INSERT INTO %swikitrust_queue (revision_id, page_id) VALUES (%s, %s) ON DUPLICATE KEY UPDATE requested_on = now(), processed = false" db_prefix (ml2int page_id) (ml2str page_title) in
    ignore (self#db_exec mediawiki_dbh s)

  (** [mark_page_as_processed page_id] marks that a page has ben processed. *)
  method mark_page_as_processed (page_id : int) : unit =
    let s = Printf.sprintf "UPDATE %swikitrust_queue SET processed = 'processed' WHERE page_id = %s" db_prefix (ml2int page_id) in
    ignore (self#db_exec mediawiki_dbh s)

  (** [mark_page_as_unprocessed page_id] marks that a page has not
      been fully processed. *)
  method mark_page_as_unprocessed (page_id : int) =
    let s = Printf.sprintf "UPDATE %swikitrust_queue SET processed = 'unprocessed' WHERE page_id = %s" db_prefix (ml2int page_id) in
    ignore (self#db_exec mediawiki_dbh s)

  (** [fetch_work_from_queue max_to_get n_retries] gets the
      list of page ids and titles that have to be brought up to date. 
      It also marks those pages as "processing", so that
      subsequent requests do not return the same revisions.  This code
      contains a transaction start / commit pair.  [max_to_get] is the
      maximum number of results to get; [n_retries] is the number of
      times the start / commit pair is used. *)
  method fetch_work_from_queue (max_to_get : int) (n_retries: int) 
    : (int * string) list =
    let n_attempts = ref 0 in 
    let results = ref [] in
    while !n_attempts < n_retries do 
      begin 
	try begin 
	  self#start_transaction;
	  let s = Printf.sprintf "SELECT page_id, page_title FROM %swikitrust_queue WHERE processed = 'unprocessed' ORDER BY requested_on ASC LIMIT %s" 
	    db_prefix (ml2int max_to_get) in
	  let get_id row : int = (not_null int2ml row.(0)) in
	  let get_title row : string = (not_null str2ml row.(1)) in
	  let get_pair row = (get_id row, get_title row) in 
	  results := Mysql.map (self#db_exec mediawiki_dbh s) get_pair;
	  let mark_as_processing (page_id, _) = 
	    let s = Printf.sprintf "UPDATE %swikitrust_queue SET processed = 'processing' WHERE page_id = %s" db_prefix (ml2int page_id) in
	    ignore (self#db_exec mediawiki_dbh s)
	  in
	  List.iter mark_as_processing !results;
          (* We need to end this loop. *)
          n_attempts := n_retries   
	end with _ -> begin 
	  (* Roll back *)
	  self#rollback_transaction;
	  n_attempts := !n_attempts + 1
	end
      end done; (* End of the multiple attempts at the transaction *)
    !results
      
  (** [erase_cached_rev_text page_id rev_id rev_time_string] does nothing; 
      it does something only in the subclass that uses the exec api. *)
  method erase_cached_rev_text (page_id: int) (rev_id: int) 
    (rev_time_string: string) : unit = ()

  (** [update_queue_page page_title] updates the default page_id to a
      real one. *)
  method update_queue_page (page_title : string) (o_page_id : int) : int =
    let s = Printf.sprintf "SELECT page_id FROM %swikitrust_page WHERE page_title = %s" 
      db_prefix (ml2str page_title) in
    let result = self#db_exec mediawiki_dbh s in
    match Mysql.fetch result with 
      None -> o_page_id
    | Some x -> begin
        let page_id = not_null int2ml x.(0) in
        let u = Printf.sprintf "UPDATE %swikitrust_queue SET page_id = %s WHERE page_title = %s" 
          db_prefix
          (ml2int page_id) (ml2str page_title) in 
        ignore (self#db_exec mediawiki_dbh u);
        page_id
      end

  (* ================================================================ *)
  (* WikiMedia Api *)

  (** Adds a page to the database.  This is normally taken care of by
      Mediawiki, except when WikiTrust is used in remote mode. *)
  method write_page (page : wiki_page_t) =
    let s = Printf.sprintf "INSERT INTO %spage (page_id, page_namespace, page_title, page_restrictions, page_counter, page_is_redirect, page_is_new, page_random, page_touched, page_latest, page_len) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) ON DUPLICATE KEY UPDATE page_latest = %s" 
      db_prefix 
      (ml2int page.page_id) 
      (ml2int page.page_namespace) 
      (ml2str page.page_title) 
      (ml2str page.page_restrictions) 
      (ml2int page.page_counter) 
      (if page.page_is_redirect then "true" else "false") 
      (if page.page_is_new then "true" else "false") 
      (ml2float page.page_random) 
      (ml2str page.page_touched) 
      (ml2int page.page_latest) 
      (ml2int page.page_len) 
      (ml2int page.page_latest)
    in
      ignore(self#db_exec mediawiki_dbh s)
(*      self#init_page page.page_id (Some page.page_title) *)

  (*
    Actually puts the text in the db, or on the filesystem.
  *)
  method private write_revision_text (rev : wiki_revision_t) =
    begin
      match rev_base_path with
	        None ->
	          let s = Printf.sprintf "INSERT INTO %stext (old_id, old_text, old_flags) VALUES (%s, %s, %s) ON DUPLICATE KEY UPDATE old_flags = %s" db_prefix (ml2int rev.revision_id) (ml2str rev.revision_content) (ml2str "utf8") (ml2str "utf8") in
	            ignore (self#db_exec mediawiki_dbh s)
        | Some b ->
	          Filesystem_store.write_revision b 
	            rev.revision_page rev.revision_id rev.revision_content;
    end

  (** [write_revision revision_to_add] adds the given data to the
      revision and text tables of the database.  It can be used to
      import data from the mediawiki api and store it locally.  *)
  method write_revision (rev : wiki_revision_t) = 
    self#write_revision_text(rev);
    (* And then the revision metadata. *)
    let s = Printf.sprintf "INSERT INTO %srevision (rev_id, rev_page, rev_text_id, rev_comment, rev_user, rev_user_text, rev_timestamp, rev_minor_edit, rev_deleted, rev_len, rev_parent_id) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s) ON DUPLICATE KEY UPDATE rev_len = %s" 
      db_prefix 
      (ml2int rev.revision_id) 
      (ml2int rev.revision_page) 
      (ml2int rev.revision_id) 
      (ml2str "") 
      (ml2int rev.revision_user) 
      (ml2str rev.revision_user_text) 
      (ml2str rev.revision_timestamp) 
      (if rev.revision_minor_edit then "true" else "false") 
      (if rev.revision_deleted then "true" else "false") 
      (ml2int rev.revision_len) 
      (ml2int rev.revision_parent_id) 
      (ml2int rev.revision_len) 
    in
    ignore (self#db_exec mediawiki_dbh s)


  (* ================================================================ *)

  (** Deletes all WikiTrust data.  (Uncolored) revisions, pages, and
      votes are not deleted.  This enables the recomputation from
      scratch of all reputations and trust.  Careful!  The
      recomputation may take a very long time for large wikis. *)
  method delete_all (really : bool) =
    let add_prefix cmd = Printf.sprintf cmd db_prefix in
    match really with
      true -> begin
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_global"));
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_page"));
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_revision"));
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_blob"));
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_user")); 
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_queue")); 
        ignore (self#db_exec mediawiki_dbh (add_prefix "TRUNCATE TABLE %swikitrust_text_cache")); 
	ignore (self#db_exec mediawiki_dbh (add_prefix "UPDATE %swikitrust_vote SET processed = FALSE")); 

        (* Note that we do NOT delete the votes!! *)
        ignore (self#db_exec mediawiki_dbh "COMMIT");
	(* We also delete the filesystem storage of signatures and
	   colored revisions. *)
	begin
	  match colored_base_path with
	    Some b -> ignore (Revision_store.delete_all b)
	  | None -> ()
	end
      end
    | false -> ignore (self#db_exec mediawiki_dbh "COMMIT")
	
end;; (* online_db *)

(** This class extends the classical db class by using the executable api to 
    get the data from the db. *)
class db_exec_api
  (db_prefix : string)
  (mediawiki_dbh : Mysql.dbd)
  (db_name: string)
  (rev_base_path: string option)
  (colored_base_path: string option)
  (max_size_per_blob: int)
  (max_revs_per_blob: int)
  (debug_mode : bool) =
  
object(self)
  inherit db db_prefix mediawiki_dbh db_name rev_base_path colored_base_path
    max_size_per_blob max_revs_per_blob debug_mode
    as super 

  (** Puts the text in the text cache, and the rest in the db using the 
      super method *)
  method private write_revision_text (rev : wiki_revision_t) =
    let s = Printf.sprintf "INSERT INTO %swikitrust_text_cache (revision_id, page_id, time_string, revision_text) VALUES (%s, %s, %s, %s) ON DUPLICATE KEY UPDATE time_string = %s" db_prefix (ml2int rev.revision_id) (ml2int rev.revision_page) (ml2str rev.revision_timestamp) (ml2str rev.revision_content) (ml2str rev.revision_timestamp) in
	    ignore (self#db_exec mediawiki_dbh s);

  (** [get_rev_text page_id text_id] returns the text associated with
      text id [text_id].  This method first tries to read the revision
      text from the cache.  If it does not succeed, it reads it from
      the exec api.  *)
  method read_rev_text (page_id: int) (rev_id: int) (text_id: int) : string =
    let s = Printf.sprintf "SELECT revision_text FROM %swikitrust_text_cache WHERE revision_id = %s"
      db_prefix (ml2int rev_id) in 
    let result = self#db_exec mediawiki_dbh s in
      match Mysql.fetch result with 
          None -> begin 
          (* No -- actally, just do the getting via WmAPI -- issue of getting 
             via page_title too annoying *)
            
	          (* Tries to read the revision using the exec API *)
	          (* NO: this is a bad idea.  
               Ian: why?
               Just say that the text can't be found, and make sure
	             the dispatcher knows that the page is not fully updated. 
               Luca, fix this. 
            *)
            
	          (*super#read_rev_text page_id rev_id text_id *)
	          (* TODO(Bo): need to switch to true execAPI *)
	          let cmdline = Printf.sprintf "%sread_rev_text -log_file /dev/null -rev_id %d" "" rev_id in
	            get_cmd_output cmdline
          end
        | Some r -> not_null str2ml r.(0)
            
  (** [erase_cached_rev_text page_id rev_id rev_time_string] erases
      the cached text of all revisions of [page_id] prior and
      including the ones for [rev_id] and [rev_time_string]. *)
  method erase_cached_rev_text (page_id: int) (rev_id: int) (rev_time_string: string) : unit =
    let s = Printf.sprintf "DELETE FROM %swikitrust_text_cache WHERE page_id = %s AND (time_string, revision_id) <= (%s, %s)" db_prefix (ml2int page_id) (ml2str rev_time_string) (ml2int rev_id) in
    ignore (self#db_exec mediawiki_dbh s)


  (**  [fetch_all_revs_after] is like the superclass method, except that it
       uses the exec api to read the revisions. *)
  method fetch_all_revs_after (req_page_id: int option) 
    (req_rev_id: int option) (timestamp : string) (rev_id: int) 
    (max_revs_to_return: int) : revision_t list =  
    (* For now it just uses the super method. *)
    super#fetch_all_revs_after req_page_id req_rev_id timestamp 
      rev_id max_revs_to_return

end (* class db_exec_api *)


(** [create_db use_exec_api db_prefix mediawiki_dbh db_name
    rev_base_path sig_base_path colored_base_path debug_mode] returns
    a db of the appropriate type, according to whether we are using
    the exec api or not.  [db_prefix] is the prefix of the db tables;
    [mediawiki_dbh] is the db handle, [db_name] is the name of the
    database (used to ensure lock uniqueness), the base paths point to
    the location of filesystem storage of revision information, and
    [debug_mode] is a flag to facilitate debugging. *)
let create_db 
    (use_exec_api: bool)
    (db_prefix : string)
    (mediawiki_dbh : Mysql.dbd)
    (db_name: string)
    (rev_base_path: string option)
    (colored_base_path: string option)
    (max_size_per_blob: int)
    (max_revs_per_blob: int)
    (debug_mode : bool) =
  if use_exec_api
  then (new db_exec_api db_prefix mediawiki_dbh db_name rev_base_path 
    colored_base_path max_size_per_blob max_revs_per_blob debug_mode)
  else (new db db_prefix mediawiki_dbh db_name rev_base_path 
    colored_base_path max_size_per_blob max_revs_per_blob debug_mode)

