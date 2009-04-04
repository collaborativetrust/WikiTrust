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

(** [api_ts2mw_ts timestamp] maps the Wikipedias api timestamp to our internal one. 
*)
let api_ts2mw_ts s =
  let ts = if Str.string_match api_tz_re s 0 then 
    (Str.matched_group 1 s) ^ (Str.matched_group 2 s) ^ (Str.matched_group 3 s)
    ^ (Str.matched_group 4 s) ^ (Str.matched_group 5 s) 
    ^ (Str.matched_group 6 s) 
  else default_timestamp in
  ts

(** [get_url url] makes a get call to [url] and returns the result as a string. *)      
let get_url (url: string) : string = 
  let call = new Http_client.get url in
  let request_header = call#request_header `Base in
  (* Accept gziped format *)
  request_header#update_field "Accept-encoding" requested_encoding_type; 
  call#set_request_header request_header;
  pipeline#add call;
  pipeline#run();
  match call#status with
    `Successful -> begin
      let body = call#response_body#value in
      match (call#response_header#content_type ()) with
      | ("text/xml",_) -> begin
	  let tmp_file = Tmpfile.new_tmp_file_name tmp_prefix in
	  Std.output_file ~filename:tmp_file ~text:body;
	  let decoded_body = Filesystem_store.read_gzipped_file tmp_file in
	  Tmpfile.remove_tmp_file tmp_file;
	  decoded_body
	end
      | _ -> body
    end
  | _ -> raise Http_client_error
;;

(** [get_xml_child node tag] returns the first child on [node] that has
    [tag], if there is one, or None if there is none. *)
let get_xml_child (node: Xml.xml) (tag: string) : Xml.xml option = 
  let l = Xml.children node in
  let rec find_first = function
      [] -> None
    | el :: rest -> if (Xml.tag el = tag) then Some el else find_first rest 
  in find_first l;;


(** [get_xml_heir node tag_list] returns the (leftmost) node reachable from
    [node] by [tag_list], if there is one, and None otherwise. *)
let rec get_xml_heir (node: Xml.xml) (tag_list: string list) : Xml.xml option =
  match tag_list with
    [] -> node
  | t :: tl -> begin
      match get_xml_child node t with
	None -> None
      | Some n -> get_xml_heir n tl
    end;;


(** [get_xml_children node tag] returns the list of children of [node] 
    that have [tag]. *)
let get_xml_children (node: Xml.xml) (tag: string) : Xml.xml string = 
  let f n t = (t = Xml.tag n) in
  List.filter f (Xml.children node)


(** [process_rev rev] takes as input a xml tag [rev], and returns 
    a wiki_revision_t stucture. *)
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
  in r


(** [process_page page] takes as input an xml tag representing a page,
    and returns a pair consisting of a wiki_page_t structure, and a 
    list of corresponding wiki_revision_t. 
   *)
let process_page (page : Xml.xml) : (wiki_page_t * wiki_revision_t list) =
  let w_page = {
    page_id = int_of_string (Xml.attrib page "pageid");
    page_namespace = (int_of_string (Xml.attrib page "ns"));
    page_title = (Xml.attrib page "title"); 
    page_restrictions = "";
    page_counter = int_of_string (Xml.attrib page "counter");
    page_is_redirect = (try ignore(Xml.attrib page "redirect"); true 
                        with Xml.No_attribute e -> false);
    page_is_new = false;
    (* For random page extraction.  The idea is just broken, of course. *) 
    page_random = (Random.float 1.0);
    page_touched = api_ts2mw_ts (Xml.attrib page "touched"); 
    page_latest = int_of_string (Xml.attrib page "lastrevid");
    page_len = int_of_string (Xml.attrib page "length")
  } in
  let rev_container = get_xml_child page "revisions" in
  let new_revs = Xml.map process_rev rev_container in
  (w_page, new_revs)


(**
   [fetch_page_and_revs after page_title rev_start_id logger], given a [page_title] 
   and a [rev_start_id], returns all the revisions of [page_title] after 
   [rev_start_id].  [logger] is, well, a logger. 
   It returns a triple, consisting of:
   - optional page info (if nothing is returned, then there is nothing to return)
   - list of revisions
   - revision id from which to start the next request; if None, 
     there are no more revisions.
   See http://en.wikipedia.org/w/api.php for more details.
*)
let fetch_page_and_revs_after (page_title : string) (rev_date : string) 
    (logger : Online_log.logger) 
    : (wiki_page_t option * wiki_revision_t list * int option) =
  let url = !Online_command_line.target_wikimedia 
    ^ "?action=query&prop=revisions|"
    ^ "info&format=xml&inprop=&rvprop=ids|flags|timestamp|user|size|comment|"
    ^ "content&rvstartid=" ^ rev_start_id ^ "&rvlimit=" ^ rev_lim
    ^ "&rvdir=newer&titles=" ^ (Netencoding.Url.encode page_title) in
  logger#log (Printf.sprintf "getting url: %s\n" url);
  let res = get_url url in
  let api = Xml.parse_string res in
  match get_xml_heir api ["query", "pages", "page"] with
    None -> (None, [], None)
  | Some page -> begin
      let (page_info, rev_info) = process_page page in
      match get_xml_child api "query-continue" with
	None -> (Some page_info, rev_info, None)
      | Some rev_cont -> begin
	  let next_rev_id = int_of_string (Xml.attrib rev_cont "rvstartid") in
	  (Some page_info, rev_info, Some next_rev_id)
	end
    end;;
      
    
(** [get_user_id user_name logger] returns the user_id of user with name [user_name]. 
    This involves querying the toolserver, which is usually heavily loaded,
    resulting in long response times.
 *)
let get_user_id (user_name : string) (logger : Online_log.logger) : int =
  let safe_user_name = Netencoding.Url.encode user_name in
  let url = !Online_command_line.user_id_server ^ "?n=" ^ safe_user_name in
  if !Online_command_line.dump_db_calls then 
    logger#log (Printf.sprintf "%s\n" url);
  let uids = ExtString.String.nsplit (get_url url) "`" in
  let uid = List.nth uids 1 in
  try int_of_string uid with int_of_string -> 0
;;
