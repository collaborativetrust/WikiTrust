(*

Copyright (c) 2009 The Regents of the University of California
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

(* Using the wikipedia API, retrieves information about pages and revisions.
 * In order for the whole system to work easily, we had to make some
 * decisions that Mediawiki seems to put off.
 * 1. we use page_id to definitively identify a page.
 * 2. page_title should be "human readable", so spaces instead of underscores
 *
 * From our point of view, it seems that Mediawiki uses page_title
 * and page_id to be interchangeable -- which isn't inappropriate since
 * they are both unique in the system.  The problem comes from the fact
 * that neither page_title nor page_id are immutable, and when they change
 * there is often a change in revisions (such as merging two articles).
 * For us to have a faithful copy of the system, we need to be defensive
 * about detecting these changes and then tossing away our old data so
 * we can be sure to properly handle merged articles.
 *
 * The problem with page_titles is that we have seen both underscores
 * and spaces used to separate words.  They probably come from different
 * APIs, but our system was previously always overwriting titles, so
 * that our database became a mish-mash of styles and only a 'LIKE'
 * search could find matching titles.  But our timing showed that our
 * LIKE query was a bit slow, so that we decided that we needed to
 * be definitive in the format here.
 *)

open Online_types;;
open Json_type;;

exception API_error of string;;
exception API_error_noretry of string;;

Random.self_init ()

let sleep_time_sec = 0
let retry_delay_sec = 120

let pipeline = new Http_client.pipeline
let buf_len = 8192
let requested_encoding_type = "gzip"
let tmp_prefix = "wiki"
let default_timestamp = "19700201000000"

let logger = Online_log.online_logger

type selector_t =
  | Title_Selector of string
  | Page_Selector of int
  | Rev_Selector of int

(* types used internally *)

(* Types used by json-static *)
type json wiki_parse_results =
    < parse "parse" :
        < ptext "text" : star;
        external_links "externallinks" : string list;
        links "links" : i_link list;
        categories "categories" : c_link list
      >
    >
and star =
    <
      star "*" : string
    >
and i_link =
  <
    namespace "ns" : int;
    star "*": string
  >
and c_link =
  <
    sort_key "sortkey": string;
    star "*": string
  >

(* Regex to map from a mediawiki api timestamp to a mediawiki timestamp
   YYYY-MM-DDTHH:MM:SS
 *)
let api_tz_re = Str.regexp "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)Z"

let ip_re = Str.regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"

(* return the \u encoding of a utf8 char if its valid, "" otherwise.  *)
let handle_invalid_utf8 substrs = 
  let u = (Pcre.get_substrings ?full_match:(Some true) substrs).(0) in 
  let m = Printf.sprintf "0x%s" (String.sub u 2 4) in
  let p = int_of_string m in
  if p <= 0xffff then 
    if (p >= 0xd800 && p < 0xe000) || (p >= 0xfffe) then "" else u  
  else u

(* match all of the \uffff style of utf8 chars *)
let utf_regex = Pcre.regexp ~flags:[] ((Pcre.quote "\\") ^ "u(.?){4}")

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
  begin
    match call#status with
      `Successful -> begin
	try
	  let encoding = call#response_header#field "Content-encoding" in
	  (* Writes the response to a temp file. *)
	  let (temp_file_name, temp_file) = Filename.open_temp_file ~mode:[Open_binary] tmp_prefix ".gz" in
	  output_string temp_file call#response_body#value;
	  close_out temp_file;
	  match encoding with
	    "gzip" -> begin
	      let decoded_body = Filesystem_store.read_gzipped_file temp_file_name in
	      Unix.unlink temp_file_name;
	      match decoded_body with
		Some str -> str
	      | None -> raise (API_error ("get_url: no body for " ^ url))
	    end
	  | _ -> raise (API_error ("get_url: unknown encoding for " ^ url))
	with Not_found -> call#response_body#value
      end
    | _ -> raise (API_error ("get_url: bad result from url " ^ url))
  end

type result_tree =
  | JSON of Json_type.t

let get_children (node: result_tree): (string * result_tree) list =
  match node with
    JSON jnode -> begin
      let jsonify (k, v) = (k, JSON v) in
      let jsonify2 v = ("", JSON v) in
      match jnode with
	  Object children -> List.map jsonify children
	| Array children -> List.map jsonify2 children
	| _ -> raise (API_error "get_children: unknown node type")
    end

(** [get_child node tag] returns the first child on [node] that has
    [tag], if there is one, or None if there is none. *)
let get_child (node: result_tree) (tag: string) : result_tree option =
  let l = get_children node in
  let rec find_first = function
      [] -> None
    | (k, v) :: rest -> if (k = tag) then Some v else find_first rest
  in find_first l


(** [get_xml_hier node tag_list] returns the (leftmost) node reachable from
    [node] by [tag_list], if there is one, and None otherwise. *)
let rec get_descendant (node: result_tree) (tag_list: string list) : result_tree option =
  match tag_list with
    [] -> Some node
  | t :: tl -> begin
      match get_child node t with
	None -> None
      | Some n -> get_descendant n tl
    end

let err_get_property (key: string) (msg: string) =
  let errmsg = Printf.sprintf "get_property: key=[%s], %s" key msg in
  raise (API_error errmsg)

let get_property (node: result_tree) (key: string) (defvalfunc): string =
  match node with
    | JSON jnode -> begin
	match jnode with
	  | Object proplist ->
	      let rec find_first = function
		  [] -> defvalfunc key "property not found"
		| (k, v) :: rest -> begin
		    if k = key then
		      match v with
			| Int i -> string_of_int i
			| String s -> s
			| Bool b ->
                            if b then (err_get_property key "TRUE type")
                            else ""
			| _ -> err_get_property key "unknown base type"
		    else find_first rest
		  end
	      in find_first proplist
	  | _ -> err_get_property key "unknown type"
      end


(** [get_text node] looks up the text from the result tree.
    If the text is not found, we check to see if the text was hidden.
    If so, the empty string will be returned; otherwise, an error is raised. *)
let get_text (node: result_tree) : string =
  let check_hidden key msg = get_property node "texthidden" err_get_property in
  get_property node "*" check_hidden

(** [get_user node] looks up the username from the result tree.
    If the user is not found, we check to see if the user was hidden.
    If so, "0.0.0.0" will be returned; otherwise, an error is raised.
    This works because the 'userhidden' property returns the empty string.  *)
let get_user (node: result_tree) : string =
  let user_hidden () = get_property node "userhidden" err_get_property in
  let check_hidden key msg = "0.0.0.0" ^ (user_hidden ()) in
  get_property node "user" check_hidden

(** [get_title node] looks up the title from the result tree.
    We expect the title to always be there.
    If the title is not there, let us see if the WpAPI tells us that
    the page is missing.  If WpAPI doesn't say missing, let's allow retry. *)
let get_title (node: result_tree) : string =
  let missing () = get_property node "missing" err_get_property in
  let check_missing key msg =
    if missing () = "" then raise (API_error_noretry "No such page")
    else raise (API_error "Unexpected result")
  in
  get_property node "title" check_missing

(** [process_rev rev] takes as input a xml tag [rev], and returns
     wiki_revision_t stucture. *)
let process_rev ((key, rev) : (string * result_tree)) : wiki_revision_t =
  let empty_string key msg = "" in
  let zero key msg = "0" in
  let revid = int_of_string (get_property rev "revid" err_get_property) in
  let minor_attr = get_property rev "minor" empty_string in
  let r = {
    revision_id = revid;
    revision_page = 0;
    revision_text_id = revid;
    revision_comment = get_property rev "comment" empty_string;
    revision_user = -1;
    revision_user_text = get_user rev;
    revision_timestamp = api_ts2mw_ts (get_property rev "timestamp" err_get_property);
    revision_minor_edit = if minor_attr = "" then false else raise (API_error_noretry "process_rev: minor edit has value");
    revision_deleted = false;
    revision_len = int_of_string (get_property rev "size" zero);
    revision_parent_id = 0;
    revision_content = get_text rev;
  }
  in r

let check_for_download_error ((key, page): (string * result_tree)) =
  if key = "-1" then raise (API_error_noretry "check_for_download_error: got -1")
  else (key, page)

(** [process_page page] takes as input a structure representing a page,
    and returns a pair consisting of a wiki_page_t structure, and a
    list of corresponding wiki_revision_t.
   *)
let process_page ((key, page): (string * result_tree))
	: (wiki_page_t * wiki_revision_t list) =
  let underscores2spaces str = ExtString.String.map
      (fun c -> if c = '_' then ' ' else c) str in
  let empty_string key msg = "" in
  let redirect_attr = get_property page "redirect" empty_string in
  let api_pageid = int_of_string (get_property page "pageid" err_get_property) in
  let api_title = underscores2spaces (get_title page) in
  let w_page = {
    page_id = api_pageid;
    page_namespace = int_of_string (get_property page "ns" err_get_property);
    page_title = api_title;
    page_restrictions = "";
    page_counter = int_of_string (get_property page "counter" err_get_property);
    page_is_redirect = if redirect_attr = "" then false
                       else true;
    page_is_new = false;
    (* For random page extraction.  The idea is just broken, of course. *)
    page_random = (Random.float 1.0);
    page_touched = api_ts2mw_ts (get_property page "touched" err_get_property);
    page_latest = int_of_string (get_property page "lastrevid" err_get_property);
    page_len = int_of_string (get_property page "length" err_get_property)
  } in
  let rev_container = get_child page "revisions" in
  match rev_container with
      None -> (w_page, [])
    | Some rev_node ->
	let revlist = get_children rev_node in
	let new_revs = List.map process_rev revlist in
	(w_page, new_revs)

let title_selector (page_title : string) (rev_start_id : int)
		(rev_lim : int) : string =
    "titles=" ^ (Netencoding.Url.encode page_title)
    ^ "&rvdir=newer"
    ^ "&rvstartid=" ^ (string_of_int rev_start_id)
    ^ "&rvlimit=" ^ (string_of_int rev_lim)

let page_selector (page_id : int) (rev_start_id : int)
		(rev_lim : int) : string =
    "pageids=" ^ (Netencoding.Url.encode (string_of_int page_id))
    ^ "&rvdir=newer"
    ^ "&rvstartid=" ^ (string_of_int rev_start_id)
    ^ "&rvlimit=" ^ (string_of_int rev_lim)

let rev_selector (rev_id : int) : string =
    "revids=" ^ (Netencoding.Url.encode (string_of_int rev_id))

let fetch_page_and_revs_after_json (selector : string) : result_tree =
  let url = !Online_command_line.target_wikimedia
    ^ "?action=query&prop=revisions|"
    ^ "info&format=json&inprop=&rvprop=ids|flags|timestamp|user|size|comment|"
    ^ "content&"
    (* ^ "rvexpandtemplates=1&"   -- even =0 triggers template expansion! *)
    ^ "&" ^ selector in
  !logger#debug 5 (Printf.sprintf "getting url: %s\n" url);
  let res = get_url url in
  try (
    (* Make sure res is properly encoded *)
    try 
      let api = Json_io.json_of_string res in
      (* logger#log (Printf.sprintf "result: %s\n" res); *)
      JSON api
    with
    | Failure e -> (
        let utf8 = Pcre.substitute_substrings ?rex:(Some utf_regex) ~subst:handle_invalid_utf8 res in
        let api = Json_io.json_of_string utf8 in
        JSON api
      )
  ) with
  | Failure e -> (
      Printf.eprintf "JSON Error: %s\nOn %s\nExc %s\nBacktrace: %s" e url
	(Printexc.to_string (Failure e))
        (Printexc.get_backtrace ());
      raise (API_error_noretry e) 
    )
      (* this means that there are certain revs we can not download -- 
	 example itwiki-Roma page. *)

(**
   [fetch_page_and_revs after selector rev_start_id db], given a [selector]
   and a [rev_start_id], returns all the revisions of [selector] after
   [rev_start_id].
   It returns a triple, consisting of:
   - optional page info (if nothing is returned, then there is nothing to return)
   - list of revisions
   - revision id from which to start the next request; if None,
     there are no more revisions.
   See http://en.wikipedia.org/w/api.php for more details.
*)
let fetch_page_and_revs_after (selector : string)
	    : (wiki_page_t * wiki_revision_t list * int option) =
  let api = fetch_page_and_revs_after_json selector in
  match get_descendant api ["query"; "pages"] with
    None -> raise (API_error_noretry "No page data in result")
  | Some pages -> begin
      let pagelist = get_children pages in
      let first = List.hd pagelist in
      let validfirst = check_for_download_error first in
      let (page_info, rev_info) = process_page validfirst in
      let nextrevprop = get_descendant api ["query-continue"; "revisions"] in
      let nextrevopt = match nextrevprop with
	| None -> None
	| Some rev_cont ->
	    let next_rev_id = int_of_string (get_property rev_cont "rvstartid" err_get_property) in
	    Some next_rev_id
      in (page_info, rev_info, nextrevopt)
    end


(** [get_user_id user_name db]
    Returns the user id of the user name if we have it.
*)
let get_user_id (user_name: string) (db: Online_db.db) : int =
  try
    if Str.string_match ip_re user_name 0 then 0
    else db#get_user_id user_name
  with 
  | Online_db.DB_Not_Found -> (
      try
	db#write_user_id user_name
      with
      | int_of_string -> 0
    )
  | Mysql.Error e -> (
      try
	db#write_user_id user_name
      with
      | int_of_string -> 0
    )

(** [get_remote_user_id user_name]
    Fetches the user id from a web service.
*)
let get_remote_user_id (user_name: string) : int =
  if Str.string_match ip_re user_name 0 then 0
  else begin
    let safe_name = Netencoding.Url.encode user_name in
    let url = !Online_command_line.user_id_server ^ "?n=" ^ safe_name in
    !logger#debug 9 (Printf.sprintf "userId lookup: %s\n" url);
    let uids = ExtString.String.nsplit (get_url url) "`" in
    let uid = List.nth uids 1 in
    try int_of_string uid with int_of_string -> 0
  end

(**
   [get_revs_from_api page_title last_id db 0] reads
   a group of revisions of the given page (usually something like
   50 revisions, see the Wikimedia API) from the Wikimedia API,
   and returns:
   - the page structure 
   - a list of revision structures
   - an optional id of the next revision to read.  Is None, then
     all revisions of the page have been read.
   Raises API_error if the API is unreachable.
*)
let rec get_revs_from_api
    (selector : selector_t) (last_id: int)
    (rev_lim: int) =
  let error_page_ident = match selector with
      | Title_Selector ts -> ts
      | Page_Selector is -> (Printf.sprintf "PageId %d" is)
      | Rev_Selector is -> (Printf.sprintf "RevId %d" is)
  in
  let sel = match selector with
    | Title_Selector ts -> title_selector ts last_id rev_lim
    | Page_Selector is -> page_selector is last_id rev_lim
    | Rev_Selector is -> rev_selector is
  in
  let resultopt =
    try
      if rev_lim = 0 then raise (API_error_noretry "get_revs_from_api: illegal rev_lim of zero");
      !logger#debug 2 (Printf.sprintf "Getting revs from api for page '%s'\n" error_page_ident);
      Some (fetch_page_and_revs_after sel)
    with
    | API_error msg -> begin
	if rev_lim < 2 then
	  raise (API_error ("get_revs_from_api: no good rev_lim available:" ^ msg))
	else
	  None
      end
  in
  match resultopt with
  | Some res -> res
  | None -> begin
      Unix.sleep retry_delay_sec;
      get_revs_from_api selector last_id (rev_lim / 2);
    end

let store_wiki_revs  (db: Online_db.db) (wiki_page: wiki_page_t) (wiki_revs: wiki_revision_t list) : unit =
    (* we standardize on using space chars instead of underscores. *)
    let update_and_write_rev rev =
      rev.revision_page <- wiki_page.page_id;
      rev.revision_user <- (get_user_id rev.revision_user_text db);
      !logger#debug 9 (Printf.sprintf "Writing to db revision %d.\n" rev.revision_id);
      db#write_revision rev
    in
    let delete_oldpage page_id =
      let got_it = db#get_page_lock page_id Online_command_line.lock_timeout in
      begin
        if got_it = false then
          raise (API_error "store_wiki_revs: DB lock failed");
	try
	  db#delete_page page_id;
	  db#release_page_lock page_id
	with e -> begin
	  db#release_page_lock page_id;
	  raise e
	end
      end
    in
    let check_bad_namespace wpage =
      if wpage.page_namespace <> 0 then begin
        !logger#log (Printf.sprintf "Page '%s' in wrong namespace.\n"
                       wpage.page_title);
        (* we don't want wrong-namespace pages on our system *)
	delete_oldpage wpage.page_id;
        raise (API_error_noretry "store_wiki_revs: wrong namespace");
      end
    in
    let check_changed_title wpage =
	let old_titles = db#get_mwpage_title wpage.page_id in
	let same_title t =
	  if t <> wpage.page_title then begin
	    !logger#log (Printf.sprintf "Page %d changed title: '%s' -> '%s'.\n"
		   wpage.page_id t wpage.page_title);
	    delete_oldpage wpage.page_id;
	    (* We need to throw an exception here, because we need to
	     * re-download the revisions of this page from the begining. *)
	    raise (API_error "store_wiki_revs: title changed");
	  end
	in
	List.iter same_title old_titles
    in
    let check_changed_pageid wpage =
	let old_pageids = db#get_mwpage_id wpage.page_title in
	let same_id i =
	  if i <> wpage.page_id then begin
	    !logger#log (Printf.sprintf "Page '%s' changed id: %d -> %d.\n"
		   wpage.page_title i wpage.page_id);
	    (* delete both the old and the new id, so we can start fresh *)
	    delete_oldpage i;
	    delete_oldpage wpage.page_id;
	    (* We need to throw an exception here, because we need to
	     * re-download the revisions of this page from the begining. *)
	    raise (API_error "store_wiki_revs: pageid changed");
	  end
	in
	List.iter same_id old_pageids
    in begin
      (* ASSERT: wiki_page.page_title uses spaces (' ') instead of
       * underscores, for a human readable title *)
      (* check for all the things that might go wrong.
       * Changed pageids first, since this lets us fix the most
       * broken records, then namespace, then title. *)
      check_changed_pageid wiki_page;
      check_bad_namespace wiki_page;
      check_changed_title wiki_page;
      (* Write the updated or new page info to the page table. *)
      !logger#debug 1 (Printf.sprintf "Got page titled %s\n" wiki_page.page_title);
      (* Write the new page to the page table. *)
      db#write_page wiki_page;
      (* Writes the revisions to the db. *)
      List.iter update_and_write_rev wiki_revs;
      ()
    end

let rec download_page_starting_with (db: Online_db.db) (title: string)
	(last_rev: int) (prev_last_rev: int) : unit =
  let (wiki_page, wiki_revs, next_rev) = get_revs_from_api (Title_Selector title) last_rev 50 in
  begin
    store_wiki_revs db wiki_page wiki_revs;
    Unix.sleep sleep_time_sec;
    match next_rev with
      | Some next_id -> begin
	  if next_id = prev_last_rev then begin
	    !logger#debug 1 (Printf.sprintf "Not making forward progress -- giving up on rev %d of page %s"
                next_id title);
	    raise (API_error_noretry "download_page_starting_with: no forward progress");
	  end else begin
	    !logger#debug 9 (Printf.sprintf "Loading next batch: %s -> %d\n" title next_id);
	    download_page_starting_with db title next_id last_rev
	  end
	end
      | None -> ()
  end

let rec download_page_starting_with_from_id (db: Online_db.db) (page_id: int)
    (last_rev: int) (prev_last_rev: int) (n_revs_downloaded : int) =
  let (wiki_page, wiki_revs, next_rev) = get_revs_from_api (Page_Selector page_id) last_rev 50 in
    begin
      let n_new_revs_downloaded = List.length wiki_revs in
	store_wiki_revs db wiki_page wiki_revs;
	Unix.sleep sleep_time_sec;
	match next_rev with
	  | Some next_id -> begin
	      if next_id = prev_last_rev then (
		!logger#debug 1 (Printf.sprintf 
		    "Not making forward progress -- giving up on rev %d of page %d"
                    next_id page_id);
		n_revs_downloaded
	      ) else (
		!logger#debug 9 (Printf.sprintf 
		    "Loading next batch: %d -> %d\n" 
		    page_id next_id);
		download_page_starting_with_from_id db page_id next_id 
		  last_rev (n_revs_downloaded + n_new_revs_downloaded)
	      )
	    end
	  | None -> (n_revs_downloaded + n_new_revs_downloaded)
    end

(** Downloads all revisions of a page, given the title, and sticks them into the db. *)
let download_page_from_id ?sid:(start_id=None) (db: Online_db.db) (page_id : int) : int =
  let lastid =
    try
      (db#get_latest_rev_id_of_page page_id) + 1
    with Online_db.DB_Not_Found -> 0
  in
  let actual_lastid =
    match start_id with
      | None -> lastid
      | Some bid -> if bid > lastid then bid else lastid
  in
  download_page_starting_with_from_id db page_id actual_lastid 0 0

(**
  Render the html using the wikimedia api
*)
let render_revision (rev_text : string) : string =
  let api_vars = [
    ("action", "parse");
    ("format", "json");
    ("text", rev_text);
  ] in
  let raw_file = List.rev (ExtString.String.nsplit
    (Http_client.Convenience.http_post !Online_command_line.target_wikimedia api_vars)
    "\n")
  in
  let json_file = Json_io.json_of_string (List.hd raw_file) in
  let cml_res = wiki_parse_results_of_json json_file in
  let pres = cml_res#parse in
  let tres = pres#ptext in
  let rendered = tres#star in
    rendered
