(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye

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

(* Using the wikipedia API, retrieves information about pages and revisions *)

open Http_client;;
open ExtLib;;
open Gzip;;
open Xml;;

type wiki_page = {
  page_id : int;
  page_namespace : int;
  page_title : string; 
  page_restrictions : string;
  page_counter : int;
  page_is_redirect : bool;
  page_is_new : bool;
  page_random : float;
  page_touched : string; 
  page_latest : int;
  page_len : int
}

type wiki_revision = {
  rev_id : int;
  rev_page : int;
  rev_text_id : int;
  rev_comment : string;
  rev_user : int;
  rev_user_text : string;
  rev_timestamp : string;
  rev_minor_edit : bool;
  rev_deleted : bool;
  rev_len : int;
  rev_parent_id : int;
  rev_content : string;
}

exception Http_client_error

let pipeline = new pipeline
let buf_len = 8192
let requested_encoding_type = "gzip"
let tmp_prefix = "wiki"

(* Given an input channel, return a string representing all there is
   to be read of this channel. *)
let input_all ic =
  let rec loop acc total buf ofs =
    let n = input ic buf ofs (buf_len - ofs) in
      if n = 0 then
	let res = String.create total in
	let pos = total - ofs in
	let _ = String.blit buf 0 res pos ofs in
	let coll pos buf =
          let new_pos = pos - buf_len in
            String.blit buf 0 res new_pos buf_len;
            new_pos in
	let _ = List.fold_left coll pos acc in
	  res
      else
	let new_ofs = ofs + n in
	let new_total = total + n in
	  if new_ofs = buf_len then
            loop (buf :: acc) new_total (String.create buf_len) 0
	  else loop acc new_total buf new_ofs in
    loop [] 0 (String.create buf_len) 0
      
let run_call url = 
  let call = new get url in
  let request_header = call # request_header `Base in
    (* Accept gziped format *)
    request_header # update_field "Accept-encoding" requested_encoding_type; 
    call # set_request_header request_header;
    pipeline # add call;
    pipeline # run();
    match call # status with
      | `Successful -> (
	  let body = call # response_body # value in
	  let repsponse_header = call # response_header in
	    Printf.printf "content_type: %s\n" 
	      (let cnt,_ = (repsponse_header # content_type ()) in cnt);
	    match (repsponse_header # content_type ()) with
	      | ("text/xml",_) -> (
		  let tmp_file = Tmpfile.new_tmp_file_name tmp_prefix in
		    Std.output_file ~filename:tmp_file ~text:body;
		    let in_chan = Gzip.open_in tmp_file in
		    let decoded_body = input_all in_chan in
		      Gzip.close_in in_chan;
		      Tmpfile.remove_tmp_file tmp_file;
		      decoded_body
		)
	      | _ -> body
	)
      | _ -> raise Http_client_error
;;

let process_rev (rev : xml) : wiki_revision =
  let w_rev = {
    rev_id = int_of_string (Xml.attrib rev "revid");
    rev_page = 0;
    rev_text_id = 0;
    rev_comment = (Xml.attrib rev "comment");
    rev_user = -1;
    rev_user_text = (Xml.attrib rev "user");
    rev_timestamp = (Xml.attrib rev "timestamp");
    rev_minor_edit = false;
    rev_deleted = false;
    rev_len = 0;
    rev_parent_id = 0;
    rev_content = (Xml.to_string (List.hd (Xml.children rev)));
  } in
    w_rev

let process_page (page : xml) : (wiki_page * wiki_revision list) =
  let w_page = {
    page_id = int_of_string (Xml.attrib page "pageid");
    page_namespace = (int_of_string (Xml.attrib page "ns"));
    page_title = (Xml.attrib page "title"); 
    page_restrictions = "";
    page_counter = int_of_string (Xml.attrib page "counter");
    page_is_redirect = false;
    page_is_new = false;
    page_random = (Random.float 1.0);
    page_touched = (Xml.attrib page "touched"); 
    page_latest = int_of_string (Xml.attrib page "lastrevid");
    page_len = int_of_string (Xml.attrib page "length")
  } in
  let revs = Xml.children page in
    (w_page, (Xml.map process_rev (List.hd revs)))

let fetch_revs (rev_ids : int list) : ((wiki_page * wiki_revision list) list) =
  let url = "http://en.wikipedia.org/w/api.php?action=query&prop=revisions|info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|content&revids=43005" in
  let res = run_call url in
  let api = Xml.parse_string res in
  let query = Xml.children (api) in
  let pages = Xml.children (List.hd query) in
    Xml.map process_page (List.hd pages)
;;
    
