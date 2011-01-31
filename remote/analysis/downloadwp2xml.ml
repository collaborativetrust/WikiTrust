(*

Copyright (c) 2009-2010 The Regents of the University of California
All rights reserved.

Authors: B. Thomas Adler, Ian Pye, Luca de Alfaro

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

open Online_command_line
open Online_types

exception Bad_Line of string

let sleep_time_sec = 1
let custom_line_format = [] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: downloadwp [options]";;

 
let tabsplit = Str.split_delim (Str.regexp "\t") in

let splitLine2TitleRev line =
    let vals = tabsplit line in
    match vals with
	| [title; rev] -> (title, (int_of_string rev))
	| [title] -> (title, 0)
	| _ -> raise (Bad_Line line)
    in

let xmlEscape (s: string) =
  (* TODO(Bo): Is this everything? *)
  let amp = Str.global_replace (Str.regexp "&") "&amp;" s in
  let quot = Str.global_replace (Str.regexp "\"") "&quot;" amp in
  let lt = Str.global_replace (Str.regexp "<") "&lt;" quot in
  let rt = Str.global_replace (Str.regexp ">") "&amp;" lt in
  rt
in

let dump_rev (r: wiki_revision_t) =
  (* TODO(Bo): Need to cache results from get_remote_user_id *)
  let uid = Wikipedia_api.get_remote_user_id r.revision_user_text in
  let tf = Timeconv.time_string_to_float r.revision_timestamp in
  let (yy, mm, dd, h, m, s) = Timeconv.float_to_time tf in 
  begin
    print_endline "<revision>";
    Printf.printf "  <id>%d</id>\n" r.revision_id;
    Printf.printf "  <timestamp>%d-%d-%dT%d:%d:%dZ</timestamp>\n" yy mm dd h m s;
    print_endline "  <contributor>";
    if uid <> 0 then begin
      Printf.printf "  <username>%s</username>\n" (xmlEscape r.revision_user_text);
      Printf.printf "  <id>%d</id>\n" uid;
    end else
      Printf.printf "  <ip>%s</ip>\n" (xmlEscape r.revision_user_text);
    print_endline "  </contributor>";
    Printf.printf "  <comment>%s</comment>\n" (xmlEscape r.revision_comment);
    Printf.printf "  <text xml:space=\"preserve\">%s</text>\n" (xmlEscape r.revision_content);
    print_endline "</revision>";
  end
in

let rec download_helper (title: string)
	(last_rev: int) (prev_last_rev: int) : unit =
  let (wiki_page, wiki_revs, next_rev) = Wikipedia_api.get_revs_from_api
	(Wikipedia_api.Title_Selector title) last_rev 50 in
  begin
    List.iter dump_rev wiki_revs;
    Unix.sleep sleep_time_sec;
    match next_rev with
      | Some next_id -> begin
	  if next_id = prev_last_rev then begin
	    (!Online_log.online_logger)#log (Printf.sprintf "Not making forward progress -- giving up");
	    raise (Wikipedia_api.API_error_noretry "download_page_starting_with: no forward progress");
	  end else begin
	    (!Online_log.online_logger)#log (Printf.sprintf "Loading next batch: %s -> %d\n" title next_id);
	    download_helper title next_id last_rev
	  end
	end
      | None -> ()
  end
in

let rec download_page_starting_with (title: string)
	(last_rev: int) (prev_last_rev: int) : unit =
  let (wiki_page, wiki_revs, next_rev) = Wikipedia_api.get_revs_from_api
	(Wikipedia_api.Title_Selector title) last_rev 50 in
  begin
    print_endline "<page>";
    Printf.printf "  <title>%s</title>" (xmlEscape wiki_page.page_title);
    Printf.printf "  <id>%d</id>" wiki_page.page_id;
    List.iter dump_rev wiki_revs;
    Unix.sleep sleep_time_sec;
    match next_rev with
      | Some next_id -> begin
	  if next_id = prev_last_rev then begin
	    (!Online_log.online_logger)#log (Printf.sprintf "Not making forward progress -- giving up");
	    raise (Wikipedia_api.API_error_noretry "download_page_starting_with: no forward progress");
	  end else begin
	    (!Online_log.online_logger)#log (Printf.sprintf "Loading next batch: %s -> %d\n" title next_id);
	    download_helper title next_id last_rev
	  end
	end
      | None -> begin
	  print_endline "</page>";
	  ();
	end
  end
in

let write_header () =
  begin
    print_endline "<mediawiki xmlns=\"http://www.mediawiki.org/xml/export-0.3/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.mediawiki.org/xml/export-0.3/ http://www.mediawiki.org/xml/export-0.3.xsd\" version=\"0.3\" xml:lang=\"en\">";
    print_endline "<siteinfo>";
    print_endline "<sitename>Wikipedia</sitename>";
    print_endline "    <base>http://en.wikipedia.org/wiki/Main_Page</base>";
    print_endline "    <generator>MediaWiki 1.12alpha</generator>";
    print_endline "    <case>first-letter</case>";
    print_endline "      <namespaces>";
    print_endline "      <namespace key=\"-2\">Media</namespace>";
    print_endline "      <namespace key=\"-1\">Special</namespace>";
    print_endline "      <namespace key=\"0\" />";
    print_endline "      <namespace key=\"1\">Talk</namespace>";
    print_endline "      <namespace key=\"2\">User</namespace>";
    print_endline "      <namespace key=\"3\">User talk</namespace>";
    print_endline "      <namespace key=\"4\">Wikipedia</namespace>";
    print_endline "      <namespace key=\"5\">Wikipedia talk</namespace>";
    print_endline "      <namespace key=\"6\">Image</namespace>";
    print_endline "      <namespace key=\"7\">Image talk</namespace>";
    print_endline "      <namespace key=\"8\">MediaWiki</namespace>";
    print_endline "      <namespace key=\"9\">MediaWiki talk</namespace>";
    print_endline "      <namespace key=\"10\">Template</namespace>";
    print_endline "      <namespace key=\"11\">Template talk</namespace>";
    print_endline "      <namespace key=\"12\">Help</namespace>";
    print_endline "      <namespace key=\"13\">Help talk</namespace>";
    print_endline "      <namespace key=\"14\">Category</namespace>";
    print_endline "      <namespace key=\"15\">Category talk</namespace>";
    print_endline "      <namespace key=\"100\">Portal</namespace>";
    print_endline "      <namespace key=\"101\">Portal talk</namespace>";
    print_endline "    </namespaces>";
    print_endline "  </siteinfo>";
  end
in
let write_footer () =
  begin
    print_endline "</mediawiki>"
  end
in

let main_loop () =
  let logger = ref (new Online_log.logger stderr true) in
  !Online_log.online_logger <- !logger;
  try
    while true do begin
      let line = input_line stdin in
      let (title, start_rev) = splitLine2TitleRev line in
      try
	write_header ();
	download_page_starting_with title start_rev 0;
	write_footer ();
      with
	Wikipedia_api.API_error msg ->
	  (!Online_log.online_logger)#log (Printf.sprintf "ERROR: %s\nmsg=%s\n" title msg);
    end done
  with End_of_file -> ()
in

main_loop ()
