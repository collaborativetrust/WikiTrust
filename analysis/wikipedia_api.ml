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
open Online_types;;

exception Http_client_error

Random.self_init ()

let pipeline = new pipeline
let buf_len = 8192
let requested_encoding_type = "gzip"
let tmp_prefix = "wiki"
let rev_lim = "50"
let target_wikimedia = "http://en.wikipedia.org/w/api.php"

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
    revision_id = int_of_string (Xml.attrib rev "revid");
    revision_page = 0;
    revision_text_id = int_of_string (Xml.attrib rev "revid");
    revision_comment = (try (Xml.attrib rev "comment") 
		   with Xml.No_attribute e -> "");
    revision_user = -1;
    revision_user_text = (Xml.attrib rev "user");
    revision_timestamp = (Xml.attrib rev "timestamp");
    revision_minor_edit = (try ignore(Xml.attrib rev "minor"); true 
		      with Xml.No_attribute e -> false);
    revision_deleted = false;
    revision_len = (try int_of_string (Xml.attrib rev "size") with Xml.No_attribute e -> 0);
    revision_parent_id = 0;
    revision_content = (Xml.to_string (List.hd (Xml.children rev)));
  } in
    w_rev

let process_page (page : xml) : (wiki_page option * wiki_revision list) =
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
    (Some w_page, (Xml.map process_rev (List.hd revs)))

let fetch_page_and_revs_after (page_title : string) (rev_date : string) : (wiki_page option * wiki_revision list) =
 
  let url = target_wikimedia ^ "?action=query&prop=revisions|"
    ^ "info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|"
    ^ "content&rvstart=" ^ rev_date ^ "&rvlimit=" ^ rev_lim
    ^ "&rvdir=newer&titles=" ^ page_title in

 (* let url = "http://en.wikipedia.org/w/api.php?action=query&prop=revisions|info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|content&revids=" ^ (let _,rvs = revs in rvs) in *)

(* http://en.wikipedia.org/w/api.php?action=query&prop=revisions|info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|content&rvstart=20060501000000&rvlimit=50&rvdir=newer&titles=Main%20Page *)

    print_endline url;
    let res = run_call url in
    let api = Xml.parse_string res in
    let query = Xml.children (api) in
    let poss_pages = Xml.children (List.hd query) in
    let pick_page acc page =
      if (Xml.tag page = "pages") then 
	process_page (List.hd (Xml.children page))
      else acc 
    in
      List.fold_left pick_page (None,[]) poss_pages
;;
    
