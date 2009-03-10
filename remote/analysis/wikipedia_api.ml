(*

Copyright (c) 2009 The Regents of the University of California
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

(* Using the wikipedia API, retrieves information about pages and revisions. *)

open Online_types;;

exception Http_client_error

Random.self_init ()

let pipeline = new Http_client.pipeline
let buf_len = 8192
let requested_encoding_type = "gzip"
let tmp_prefix = "wiki"
let rev_lim = "50"
let default_timestamp = "19700201000000"
(* Regex to map from a mediawiki api timestamp to a mediawiki timestamp
   YYYY-MM-DDTHH:MM:SS
 *)
let api_tz_re = Str.regexp "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)Z"

(* 
   [api_ts2mw_ts timestamp]
   Maps the Wikipedias api timestamp to our internal one. 
*)
let api_ts2mw_ts s =
  let ts = if Str.string_match api_tz_re s 0 then 
    (Str.matched_group 1 s) ^ (Str.matched_group 2 s) ^ (Str.matched_group 3 s)
    ^ (Str.matched_group 4 s) ^ (Str.matched_group 5 s) 
    ^ (Str.matched_group 6 s) 
  else default_timestamp in
    ts

(* 
   [input_all in_channel]

   Given an Gzip.in_channel, return a string representing all there is
   to be read of this gzipped file.
   This is a slup function, reading everything possible into a string and then
   returning the string.
   This is code copied from the extlib library, but converted to 
   work with an Gzip.in_channel input.
   Luca: but can the code be simplified by using the Buffer module?
*)

let input_all (ic : Gzip.in_channel) =
  let rec loop acc total buf ofs =
    let n = Gzip.input ic buf ofs (buf_len - ofs) in
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

(*
  [run_call url] makes a get call to [url] and returns the result as a string.
*)      
let run_call url = 
  let call = new Http_client.get url in
  let request_header = call # request_header `Base in
    (* Accept gziped format *)
    request_header # update_field "Accept-encoding" requested_encoding_type; 
    call # set_request_header request_header;
    pipeline # add call;
    pipeline # run();
    match call # status with
      | `Successful -> (
	  let body = call # response_body # value in
	    match (call # response_header # content_type ()) with
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

(*
  [process_rev rev] takes as input a xml tag [rev], and returns 
  a wiki_revision_t stucture.
*)
let process_rev (rev : Xml.xml) : wiki_revision_t =
  let r = {
    revision_id = int_of_string (Xml.attrib rev "revid");
    revision_page = 0;
    revision_text_id = int_of_string (Xml.attrib rev "revid");
    revision_comment = (try (Xml.attrib rev "comment") 
		   with Xml.No_attribute e -> "");
    revision_user = -1;
    revision_user_text = (Xml.attrib rev "user");
    revision_timestamp = api_ts2mw_ts (Xml.attrib rev "timestamp");
    revision_minor_edit = (try ignore(Xml.attrib rev "minor"); true 
		      with Xml.No_attribute e -> false);
    revision_deleted = false;
    revision_len = (try int_of_string (Xml.attrib rev "size") with Xml.No_attribute e -> 0);
    revision_parent_id = 0;
    revision_content = try 
      (Netencoding.Html.decode ~in_enc:`Enc_utf8 
	 ~out_enc:`Enc_utf8 () 
	 (Xml.to_string (List.hd (Xml.children rev))))
    with Failure f -> "";
  } 
  in
    r

(**
  [process_page page] takes as input an xml tag representing an optional
  page, and returns a pair consisting of a wiki_page_t structure, and a 
  list of corresponding wiki_revision_t. 

   The Some/None type of the wiki_page_t return is an error handling mechanisam.
   If there are no pages found, this is an error.
   None is set in fetch_page_and_revs_after.
   Some is set only after a page is found.
   
*)
let process_page (page : Xml.xml) : 
    (wiki_page_t option * wiki_revision_t list) =
  let w_page = {
    page_id = int_of_string (Xml.attrib page "pageid");
    page_namespace = (int_of_string (Xml.attrib page "ns"));
    page_title = (Xml.attrib page "title"); 
    page_restrictions = "";
    page_counter = int_of_string (Xml.attrib page "counter");
    page_is_redirect = (try ignore(Xml.attrib page "redirect"); true 
                        with Xml.No_attribute e -> false);
    page_is_new = false;
    page_random = (Random.float 1.0);
    page_touched = api_ts2mw_ts (Xml.attrib page "touched"); 
    page_latest = int_of_string (Xml.attrib page "lastrevid");
    page_len = int_of_string (Xml.attrib page "length")
  } in
  let revs = Xml.children page in
  let new_revs = (Xml.map process_rev (List.hd revs)) in
    if List.length new_revs > 1 then
      (Some w_page, List.tl new_revs)
    else 
      (Some w_page, [])

(**
   [fetch_page_and_revs after page_title rev_date logger], given a [page_title] 
   and a [rev_date], returns all the revisions of [page_title] after [rev_date]. 
   [logger] is, well, a logger. 
   Ian: why do you use page_title, rather than page_id? 

   Luca: This is inherent in the way the mediawiki api works -- the function I
   need is keyed off of page_title, not page_id. 
   See http://en.wikipedia.org/w/api.php for more details.
*)
let fetch_page_and_revs_after (page_title : string) (rev_date : string) 
    (logger : Online_log.logger) : (wiki_page_t option * wiki_revision_t list) =
  let url = !Online_command_line.target_wikimedia 
    ^ "?action=query&prop=revisions|"
    ^ "info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|"
    ^ "content&rvexpandtemplates=1&rvstart=" ^ rev_date ^ "&rvlimit=" ^ rev_lim
    ^ "&rvdir=newer&titles=" ^ (Netencoding.Url.encode page_title) in
    if !Online_command_line.dump_db_calls then 
      logger#log (Printf.sprintf "%s\n" url);
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
    
(** [get_user_id user_name logger] returns the user_id of user with name [user_name]. 
    This involves querying the toolserver, which is usaually heavilly loaded,
    resulting in long responce times.
 *)
let get_user_id (user_name : string) (logger : Online_log.logger) : int =
  let safe_user_name = Netencoding.Url.encode user_name in
  let url = !Online_command_line.user_id_server ^ "?n=" ^ safe_user_name in
    if !Online_command_line.dump_db_calls then 
      logger#log (Printf.sprintf "%s\n" url);
    let uids = ExtString.String.nsplit (run_call url) "`" in
    let uid = List.nth uids 1 in
      try int_of_string uid with int_of_string -> 0 in
;;
