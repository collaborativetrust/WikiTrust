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

open Online_command_line
open Online_types

let page_id = ref 0
let last_rev = ref 0
let custom_line_format = [
	("-page_id", Arg.Set_int page_id, "Page id to download info for.");
	("-last_rev", Arg.Set_int last_rev, "Starting revision.");
    ] @ command_line_format

let _ = Arg.parse custom_line_format noop "Usage: fetch_all_revs_after [options]";;

let main () =
    let (wiki_page, wiki_revs, next_id) = 
	Wikipedia_api.get_revs_from_api (Wikipedia_api.Page_Selector !page_id) !last_rev 50 in
    let process_rev rev =
	Printf.printf "%d:%d:%s\n" wiki_page.page_id rev.revision_id rev.revision_timestamp in
    List.iter process_rev wiki_revs
	(* TODO: Instead of iterating over list, we need to serialize the
	 * whole list to stdout and/or put into the text cache *)
in
main ()
