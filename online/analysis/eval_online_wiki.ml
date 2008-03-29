(*

Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, B. Thomas Adler, Vishwanath Raman, Ian Pye

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


(* This is the top-level code of the wiki online xml evaluation.  *)

let db_user = ref ""
let set_db_user u = db_user := u
let db_pass = ref ""
let set_db_pass p = db_pass := p
let db_name = ref ""
let set_db_name d = db_name := d


let command_line_format = 
  [("--db_user", Arg.String set_db_user, "<user>: DB user to use");
   ("--db_name", Arg.String set_db_name, "<name>: Name of the DB to use");
   ("--db_pass", Arg.String set_db_pass, "<pass>: DB password");
   ("--page_id", Arg.Int set_page, "<page_id>: Page");
   ("--rev_id", Arg.Int set_rev, "<rev_id>: Revision");
  ] 

let _ = Arg.parse command_line_format ;;

(* Does all the work of processing the given page and revision *)

 
