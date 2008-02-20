(*

Copyright (c) 2007-2008 The Regents of the University of California
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


(** Here are some convenience operations on text *)

type word = string;;
exception Illegal_separator
exception Text_error

(* This is the type of the returned tokenized string. *)
type sep_t = Title_start of string (* sequence to start a title *)
	     | Title_end of string (* sequence to end a title *)
	     | Par_break of string (* paragraph break *)
	     | Bullet of string (* sequence to do a bullet *)
	     | Indent of string (* sequence to indent text *)
	     | Space of string  (* normal break string *)
	     | Newline of string (* whitespace containing a \n *)
	     | Armored_char of string (* armored xml tag *)
	     | Table_line of string (* table tag that needs to be alone on a line *)
	     | Table_cell of string (* table tag for cell start *)
	     | Table_caption of string (* table caption *)
	     | Tag of string * int (* contains the tag, and the ordinal of the tag *)
	     | Word of string * int (* contains the word, and the ordinal of the word *)
	     | Redirect of string * int (* redirect tag *)

(* Regular expressions to separate words for reputation analysis *)
(* This is the regexp used to split for reputation analysis *)
let sep_regexp = Str.regexp "[.,;:=?!-]*\\([][(){}<>'#* \t\n\"|]\\|^\\|$\\)+[.,;:=?!-]*"
(* a word has to contain at least an alpha or number *)
let in_word_regexp = Str.regexp ".*[a-zA-Z0-9]"


(* **************************************************************** *)
(* Printing *)

let print_sep (s: sep_t) = 
  begin 
    match s with 
      Title_start t -> Printf.printf "Title_start(%S) " t
    | Title_end t -> Printf.printf "Title_end(%S) " t
    | Par_break t -> Printf.printf "Par_break(%S) " t
    | Bullet t -> Printf.printf "Bullet(%S) " t
    | Indent t -> Printf.printf "Indent(%S) " t
    | Space t -> Printf.printf "Space(%S) " t
    | Newline t -> Printf.printf "Newline(%S) " t
    | Armored_char t -> Printf.printf "Armored_char(%S) " t
    | Table_line t -> Printf.printf "Table_line(%S) " t
    | Table_cell t -> Printf.printf "Table_cell(%S) " t
    | Table_caption t -> Printf.printf "Table_caption(%S) " t
    | Tag (t, j) -> Printf.printf "Tag(%S,%d) " t j
    | Word (t, j) -> Printf.printf "Word(%S,%d) " t j
    | Redirect (t, j) -> Printf.printf "Redirect(%S,%d) " t j
  end

let print_seps (sep_a: sep_t array) = 
  let f i s = 
    Printf.printf "%d:" i; 
    print_sep s
  in Array.iteri f sep_a

let print_seps_vec (sep_a: sep_t Vec.t) = 
  let f i s = 
    Printf.printf "%d:" i; 
    print_sep s
  in Vec.iteri f sep_a; 
  Printf.printf "\n"

let print_words (word_a: word array) = 
  let f i s = Printf.printf "%d:%S " i s
  in Array.iteri f word_a

let print_words_and_seps (w: word array) (s: sep_t array) = 
  Printf.printf ("\n"); 
  print_words w; 
  Printf.printf ("\n"); 
  print_seps s

(* **************************************************************** *)
(* This function removes HTML comments. 
   This is much more of a pain that it should be, for two reasons. 
   First, Wikipedians violate the markup language specification, and use HTML comments
   in front of tags that should come at the beginning of the line, such as in 
   <!-- This is an obnoxious comment before a table start -->{| ... 
   Second, Wikipedians do not follow the W3C recommendation 
   http://www.w3.org/TR/html401/intro/sgmltut.html#h-3.2.4 that -- not be used inside
   an HTML comment, preventing the use of regexps to eliminate comments, as they 
   write things like: 
   <!-- Look I can make beautiful arrow that no-one sees ------->
   So I have to go through this.  Ah well. 
 *)
let start_comment_r = Str.regexp "<!--"
let end_comment_r = Str.regexp "--[ \n\t]*>" 

let remove_html_comments (text: string) : string = 
  let t = ref text in 
  let start_pos = ref 0 in 
  let t_len = ref (String.length text) in 
  while !start_pos < !t_len do begin 
    (* Looks for a start of comment *)
    let start_comment_pos_opt = 
      try Some (Str.search_forward start_comment_r !t !start_pos) 
      with Not_found -> None 
    in 
    match start_comment_pos_opt with 
      None -> start_pos := !t_len
    | Some start_comment_pos -> begin 
	let end_start_block = Str.match_end () in 
	(* Found the beginning of a comment.  Looks for the end *)
	let end_comment_pos_opt = 
	  try Some (Str.search_forward end_comment_r !t end_start_block) 
	  with Not_found -> None 
	in 
	match end_comment_pos_opt with 
	  None -> start_pos := !t_len
	| Some end_comment_pos -> begin 
	    let end_pos = Str.match_end () in 
	    (* now the portion from !start_pos to end_pos must be cut out! *)
	    t := (Str.string_before !t start_comment_pos) ^ 
	      (Str.string_after !t end_pos); 
	    t_len := !t_len - (end_pos - start_comment_pos); 
	    start_pos := start_comment_pos
	  end (* found the end *)
      end (* found the beginning *)
  end done; (* while loop *)
  !t

(* **************************************************************** *)
(* Splitting text into words, for reputation analysis *)

(* This function uses stack space proportional to the number of words 
   into which the text is split, but it is fast. It returns a word Vec, 
   so that there is no overflow danger. *)
let split_string_into_word_vec (text: string) : word Vec.t = 
  (* Splits the text *)
  let wl = Str.split sep_regexp text in 
  (* filters out the empty words and the ones which do not
     contain a number or letter *)
  let f w = (
    (String.length w) > 0 
    && (Str.string_match in_word_regexp w 0)) in 
  Vec.of_list (List.filter f wl)

(* This function splits a Vec of strings into an array of words. 
   The function takes care in the appropriate way of long strings, 
   and processes concatenations n an optimal way. *)
let split_into_words (text_v: string Vec.t) : word array = 
  (* First, we generate a word Vec.t *)
  let ve = Vec.empty in 
  let vn l d r = Vec.concat (Vec.concat l (split_string_into_word_vec d)) r in 
  let word_vect = Vec.visit_post ve vn text_v in 
  (* Then, returns an array of the result *)
  Vec.to_array word_vect


(* **************************************************************** *)
(* Splitting text into words and separators, used for text trust *)

(* This type is used internally *)
type piece_t =   WS_title_start of string
	       | WS_title_end of string
	       | WS_bullet of string
	       | WS_par_break of string 
	       | WS_indent of string
	       | WS_table_line of string
	       | WS_table_cell of string
	       | WS_table_caption of string
	       | TXT_tag of string
	       | TXT_redirect of string
	       | TXT_splittable of string 
		 


(* These are constructs with opening and closing tags, except for titles 
   (which are handled later) *)
let beg_link_1 = "\\(\\[\\[\\)"
let end_link_1 = "\\(\\]\\]\\)"
let end_link_1_r = Str.regexp end_link_1
let beg_link_2 = "\\(\\[http:\\)"
let beg_link_2_r = Str.regexp beg_link_2
let end_link_2 = "\\(\\]\\)"
let end_link_2_r = Str.regexp end_link_2
let beg_stub_3 = "\\({{\\)"
let beg_stub_3_r = Str.regexp beg_stub_3
let end_stub_3 = "\\(}}\\)"
let end_stub_3_r = Str.regexp end_stub_3
let beg_link_5 = "\\(#REDIRECT \\)"
let end_link_5_r = Str.regexp "\\(\\]\\]\\)"
let beg_tag_6  = "\\(<[a-zA-Z1-9_]+[^>]*>\\)"
let end_tag_6_r  = Str.regexp "</\\([a-zA-Z1-9_]+\\) *>"
(*
let beg_tag_7  = "\\(&lt;[a-zA-Z1-9_]+\\)"
let end_tag_7_r  = Str.regexp "&lt;/\\([a-zA-Z1-9_]+\\) *&gt;"
 *)
let tag_name_r = Str.regexp "[a-zA-Z1-9_]+"
(* let single_close_r = Str.regexp "&gt;" *)
(* Any of these openings *)
let open_pattern = Str.regexp_case_fold
  (beg_link_1 ^ "\\|" ^ beg_link_2 ^ "\\|" ^ beg_stub_3 ^ "\\|" ^ beg_link_5 ^ "\\|" ^ beg_tag_6 )

(* For matching *)
(* These are the tags that close with ]] *)
let open_close_brbr_r = Str.regexp_case_fold (beg_link_1 ^ "\\|" ^ beg_link_5)
(* These are openings or closures of tags that close with ]] *)
let match_brbr_r = Str.regexp_case_fold (beg_link_1 ^ "\\|" ^ beg_link_5 ^ "\\|" ^ end_link_1)
(* These are openings or closures of tags that close with ].  Check that it is a single ] ! *)
let match_br_r = Str.regexp (beg_link_2  ^ "\\|" ^ end_link_2)
(* These are openings or closures of tags that close with }} *)
let match_cbr_r = Str.regexp (beg_stub_3 ^ "\\|" ^ end_stub_3)

let max_nested_tags = 20
type token_t = LeftTok | OpenTag | RedirTok

(* This function takes a piece_t Vec.t, and separates out the tags (see above regular expressions)
   into atomic pieces that won't be touched by coloring. *)
let separate_string_tags (pv: piece_t Vec.t) : piece_t Vec.t = 
  (* This is the function that is iterated on text *)
  let f (piece: piece_t) (pp: piece_t Vec.t) : piece_t Vec.t = 
    match piece with 
    | TXT_splittable text -> begin 
	(* We accumulate in p the result of the first analysis. *)
	let p = ref pp in 
	(* First, searches in text for: 
	   [[ [http: {{ {| <tagname ...> 
	   that signal the beginning of blocks.  It then puts the portion of text 
	   matched in an TXT_tag, and what comes before as TXT_splittable. *)
	let start_search = ref 0 in (* i indicates where we start to search from *)
	let start_pos = ref 0 in    (* start_pos indicates where the next unprocessed char of text is *)
	let text_l = String.length text in 
	let search_again = ref true in 
	while !start_search < text_l && !search_again do begin 
	  (* Searches for any opening token *)
	  let match_start_pos_opt = 
	    try Some (Str.search_forward open_pattern text !start_search) 
	    with Not_found -> None
	  in 
	  match match_start_pos_opt with 
	    None -> begin 
              search_again := false; 
              p := Vec.append (TXT_splittable (Str.string_after text !start_pos)) !p 
	    end
	  | Some match_start_pos -> begin 
              (* Ok, there is a potential token (potential, because we don't know if 
		 the closing token is present) that begins at position j. *)
              (* match_start_pos marks the beginning of the opening token. *)
              let s = Str.matched_string text in 
              let match_end_pos = Str.match_end () in (* end of opening token *)
              (* Sets closing_token_r to the token to be found. 
		 Note that in case of single tags, we are looking for both 
		 openings and closings. *)
              let (closing_token_r, tag_kind) = 
		if s = "[[" then (match_brbr_r, LeftTok)
		else if s = "[http:" then (match_br_r, LeftTok)
		else if s = "{{" then (match_cbr_r, LeftTok)
		else if (String.uppercase s) = "#REDIRECT " then (match_brbr_r, RedirTok)
		else (end_tag_6_r, OpenTag) 
              in 
	      match tag_kind with 
		OpenTag -> begin 
		  let len_opening = 1 in 
		  let tag_name = 
		    if Str.string_match tag_name_r text (match_start_pos + len_opening) 
		    then Str.matched_string text 
		    else raise Text_error 
		  in 
		  (* It is a tag.  We need to decide whether it is a single tag
		     <tagname />  or whether it may have a closing tag, as in <tagname ... > *)
		  let before_closing = String.get text (match_end_pos - 2) in 
		  if before_closing = '/' then begin 
		    if match_start_pos > !start_pos then begin 
		      let piece_before = (String.sub text !start_pos (match_start_pos - !start_pos)) in 
		      p := Vec.append (TXT_splittable piece_before) !p
		    end; 
		    (* It is a single tag of type <.../>, we don't need to look for the closing tag *)
		    let piece_atomic = String.sub text match_start_pos (match_end_pos - match_start_pos) in 
		    p := Vec.append (TXT_tag piece_atomic) !p; 
		    (* Begins from match_end_pos the rest of the analysis *)
		    start_pos := match_end_pos; 
		    start_search := match_end_pos
		  end else begin 
		    (* It is not a single tag.  We need to find a closing tag with the same name. *)
		    let closing_found = ref false in 
		    let n_attempts = ref max_nested_tags in 
		    let start_attempt = ref match_end_pos in 
		    while !n_attempts > 0 && not !closing_found do begin 
		      let closing_start_opt = 
			try Some (Str.search_forward closing_token_r text !start_attempt)
			with Not_found -> None
		      in 
		      match closing_start_opt with 
			Some closing_start -> begin 
			  (* We must check that the names coincide *)
			  let name_found = Str.matched_group 1 text in 
			  let end_closing = Str.match_end () in 
			  if tag_name = name_found then begin 
			    (* Yes, we have found the closing tag! *)
			    closing_found := true; 
			    if match_start_pos > !start_pos then begin 
			      let piece_before = (String.sub text !start_pos (match_start_pos - !start_pos)) in 
			      p := Vec.append (TXT_splittable piece_before) !p
			    end; 
			    let piece_atomic = String.sub text match_start_pos (end_closing - match_start_pos) in 
			    p := Vec.append (TXT_tag piece_atomic) !p; 
			    (* Begins from end_closing the rest of the analysis *)
			    start_pos := end_closing; 
			    start_search := end_closing
			  end else begin 
			    (* No, the names do not match.  Looks further on. *)
			    n_attempts := !n_attempts - 1; 
			    start_attempt := end_closing
			  end
			end (* Some closing_start *)
		      | None -> n_attempts := 0 (* There is no closing tag. *)
		    end done; 
		    (* If no matching has been found, moves on, making it a singleton tag *)
		    if not !closing_found then begin 
		      if match_start_pos > !start_pos then begin 
			let piece_before = (String.sub text !start_pos (match_start_pos - !start_pos)) in 
			p := Vec.append (TXT_splittable piece_before) !p
		      end;
		      let piece_atomic = String.sub text match_start_pos (match_end_pos - match_start_pos) in 
		      p := Vec.append (TXT_tag piece_atomic) !p; 
		      start_pos := match_end_pos; 
		      start_search := match_end_pos
		    end
		  end (* case for a tag with matching closing tag *)
		end (* OpenTag and OpenCodedTag *)
	      | LeftTok | RedirTok -> begin 
		  (* Not a tag, looks for the closing token if any. Keeps track of the open/close deficit. *)
		  (* Careful: Redirect comes with an initial imbalance of 0 *)
		  let n_open = if tag_kind = LeftTok then ref 1 else ref 0 in 
		  let closing_found = ref false in 
		  let n_attempts = ref max_nested_tags in 
		  let start_attempt = ref match_end_pos in 
		  while !n_attempts > 0 && not !closing_found do begin 
		    let nextpos_opt = 
		      try Some (Str.search_forward closing_token_r text !start_attempt)
		      with Not_found -> None 
		    in 
		    match nextpos_opt with 
		      (* This is an open token with missing closed token. Discard *)
		      None -> n_attempts := 0; 
			(* Has a closing token *)
		    | Some nextpos -> begin
			let end_closing = Str.match_end () in 
			let text_tag = Str.matched_string text in 
			(* Checks whether this is an open tag *)
			if text_tag = "]]" || text_tag = "]" || text_tag = "}}" then begin 

			  (* It must be a closing match *)
			  n_open := !n_open - 1; 
			  (* Checks if matched *)
			  if !n_open = 0 then begin 
			    (* Yes, we have found the match *)
			    (* Appends the splittable portion before the token to !p *)
			    if match_start_pos > !start_pos then begin 
			      let piece_before = (String.sub text !start_pos (match_start_pos - !start_pos)) in 
			      p := Vec.append (TXT_splittable piece_before) !p
			    end; 
			    let piece_atomic = String.sub text match_start_pos (end_closing - match_start_pos) in 
			    let tag_atomic = 
			      if tag_kind = LeftTok then TXT_tag piece_atomic else TXT_redirect piece_atomic 
			    in 
			    p := Vec.append tag_atomic !p; 
			    (* Begins from end_closing the rest of the analysis *)
			    start_pos := end_closing; 
			    start_search := end_closing;
			    closing_found := true
			  end else begin 
			    (* No, it was just a submatch *)
			    start_attempt := end_closing; 
			    n_attempts := !n_attempts - 1
			  end

			end else begin 

			  (* It is an open match; skips *)
			  n_open := !n_open + 1; 
			  start_attempt := end_closing; 
			  n_attempts := !n_attempts - 1

			end (* if then else on whether it is an open or close tag *)
		      end (* Some nextpos *)
		  end done; (* while *)
		  (* If no matching has been found, moves on *)
		  if not !closing_found then start_search := match_end_pos
		end (* LeftTok *)
	    end (* there is something found *)
	end done; (* end of the while that searches the piece of text *)
	!p
      end (* TXT_splittable *)
    | _ -> Vec.append piece pp
  in  (* end of function f *)
  Vec.fold f pv Vec.empty


let title_start_e = "\\(\n==+\\)"
let title_start_r = Str.regexp title_start_e 
let title_end_e = "\\(==+[ \t]*$\\)"
let title_end_r     = Str.regexp title_end_e
let par_break_tag_e = "\\(\n[ \t]*$\\)\\|\\(\n----+[ \t]*$\\)"
let par_break_tag_r = Str.regexp par_break_tag_e 
let one_liner_r = Str.regexp (title_start_e ^ "\\|" ^ title_end_e ^ "\\|" ^ par_break_tag_e) 

(* This function separates out titles and paragraph breaks. 
   What makes titles and paragraph breaks special is that they are delimited 
   by two \n, and they need both in order to be correctly recognized. 
   So, when we match them, we swallow a \n, but we must re-insert a new \n. 
   In fact, if we did not, and an enumeration followed, our eating up the \n 
   would cause us to miss recognizing the enumeration. *)
let separate_titles (v: piece_t Vec.t) : piece_t Vec.t = 
  (* The function f is folded on v, and does the job.
     d is the piece that is to be analyzed. 
     w is where the result is accumulated. *)
  let f (d: piece_t) (w: piece_t Vec.t) : piece_t Vec.t = 
    match d with 
      TXT_tag _ | TXT_redirect _ -> Vec.append d w
    | TXT_splittable s -> begin 
        (* s is a splittable string.  Looks for titles in it *)
        let l = Str.full_split one_liner_r s in 
        (* Now, we process the list l, adding to w the results. *)
        (* Function g is folded_left on list l, and on w, 
           to produce the result. *)
        let g (ww: piece_t Vec.t) (el: Str.split_result) : piece_t Vec.t = 
          match el with 
            Str.Delim t -> begin 
	      (* There are now two cases, depending on whether we matched 
		 a paragraph break, or a title *)
	      if Str.string_match title_start_r t 0 
	      then Vec.append (WS_title_start t) ww
	      else if Str.string_match par_break_tag_r t 0 
	      then Vec.append (WS_par_break t) ww
	      else Vec.append (WS_title_end t) ww
	    end
          | Str.Text t -> Vec.append (TXT_splittable t) ww
        in (* end of function g *)
        List.fold_left g w l
      end
    | _ -> raise Text_error
  in (* end of function f *)
  Vec.fold f v Vec.empty

(* Bullets *)
let bullet_tag = "\\(\n\\*\\** *\\)"
let bullet_tag_r = Str.regexp bullet_tag
let number_tag = "\\(\n##* *\\)"
let number_tag_r = Str.regexp number_tag
(* Indent *)
let indent_tag = "\\(\n +\\)"
let indent_tag_r = Str.regexp indent_tag
(* Colon-indent *)
let colon_tag = "\\(\n::* *\\)"
let colon_tag_r = Str.regexp colon_tag
(* Tables beginnings and ends.  Note that by reading these tags till the end of the line,
   we can ensure that any formatting instructions are not broken. *)
let begin_table   = "\\(\n{|[^\n]*\\)"
let begin_table_r = Str.regexp begin_table
let end_table     = "\\(\n|}\\)"
let end_table_r   = Str.regexp end_table
let new_row       = "\\(\n|-[^\n]*\\)"
let new_row_r     = Str.regexp new_row
let table_caption = "\\(\n|\\+\\)"
let table_caption_r = Str.regexp table_caption

(* Line start stuff *)
let line_start_r = Str.regexp (bullet_tag ^ "\\|" ^ number_tag ^ "\\|" ^ indent_tag
  ^ "\\|" ^ colon_tag 
  ^ "\\|" ^ begin_table ^ "\\|" ^ end_table ^ "\\|" ^ new_row ^ "\\|" ^ table_caption)

(* This function separates out the markup language used for indentation, and
   for bullet and enumeration lists. *)
let separate_line_tags (v: piece_t Vec.t) : piece_t Vec.t = 
  (* The function f is folded on v, and does the job. 
     d is the piece that is to be analyzed. 
     w is where the result is accumulated. *)
  let f (d: piece_t) (w: piece_t Vec.t) : piece_t Vec.t = 
    match d with 
      TXT_splittable s -> begin 
        (* s is a splittable string.  Looks for line tags in it *)
        let l = Str.full_split line_start_r s in 
        (* Function g is folded_left on l, and on w, as above. *)
        let g (ww: piece_t Vec.t) (el: Str.split_result) : piece_t Vec.t = 
          match el with 
	    Str.Text t -> Vec.append (TXT_splittable t) ww
          | Str.Delim t -> begin
	      (* We must distinguish which tag has been matched *)
	      if (Str.string_match bullet_tag_r t 0) || 
		(Str.string_match number_tag_r t 0) 
	      then Vec.append (WS_bullet t) ww
	      else if (Str.string_match indent_tag_r t 0) 
	      then Vec.append (WS_indent t) ww
	      else if (Str.string_match table_caption_r t 0)
	      then Vec.append (WS_table_caption t) ww
	      else Vec.append (WS_table_line t) ww
	    end
        in List.fold_left g w l
      end
    | _ -> Vec.append d w
  in (* end of f *)
  Vec.fold f v Vec.empty


(* This processes table elements. The flow is as follows. 
   First, it locates the starts of the rows, via start_row.  Note that caption lines 
   logically should be handled here, but in practice, we have included them in the 
   above code instead. 
   After the row starts are located, there are one or more cells on the row. 
   These are divided by || or !!,  and the only problem is to take care of the format 
   modifier " || modifier | content || ". 
 *)

let new_cell_line = "\\(\n[|!]\\)"
let new_cell_cont = "\\(\\(||\\)\\|\\(!!\\)\\)"
let format_mod    = "\\([^|\n]+|[^|]\\)"
let new_cell_r    = Str.regexp (new_cell_line ^ "\\|" ^ new_cell_cont)
let format_mod_r  = Str.regexp format_mod

(* This function separates out the table tags that occur inside a table, 
   to ensure formatting is preserved. *)
let separate_table_tags (v: piece_t Vec.t) : piece_t Vec.t = 
  (* The function f is folded on v, and does the job. 
     d is the piece that is to be analyzed. 
     w is where the result is accumulated. *)
  let f (d: piece_t) (w: piece_t Vec.t) : piece_t Vec.t = 
    match d with 
      TXT_splittable s -> begin 
        (* s is a splittable string.  Looks for line tags in it *)
	(* We accumulate the result in p *)
	let p = ref w in 
	let start_pos = ref 0 in 
	let to_search = ref true in 
	while !to_search do begin 
	  (* Searches for the next cell beginning *)
	  let i_opt = try 
	      Some (Str.search_forward new_cell_r s !start_pos)
	    with Not_found -> None 
	  in match i_opt with 
	    None -> begin 
	      (* Closes the search and adds the last piece *)
	      to_search := false; 
	      p := Vec.append (TXT_splittable (Str.string_after s !start_pos)) !p
	    end
	  | Some i -> begin 
	      (* There is a cell beginning. *)
	      (* First, puts the previous stuff in the results vector *)
	      let j = Str.match_beginning () in 
	      let j' = Str.match_end () in 
	      if j > !start_pos then begin 
		p := Vec.append (TXT_splittable (String.sub s !start_pos (j - !start_pos))) !p; 
		start_pos := j
	      end; 
	      (* Now we have to find the end of the cell beginning. 
		 This is complicated by the existence of modifier cells tags, 
		 so we search for those next. *)
	      if Str.string_match format_mod_r s j' then begin 
		(* Yes, found a modifier *)
		(* Finds the end of the match.  The -1 is to compensate for the regexp. *)
		let k = (Str.match_end ()) - 1 in 
		(* Adds the cell tag, and moves on *)
		p := Vec.append (WS_table_cell (String.sub s j (k - j))) !p; 
		start_pos := k
	      end else begin 
		(* There is no modifier. Adds the cell tag, and moves on *)
		p := Vec.append (WS_table_cell (String.sub s j (j' - j))) !p; 
		start_pos := j'
	      end (* Search for the end of the cell declaration *)
	    end (* There was a match for a next cell beginning *)
	end done; (* while loop; the result is in !p *)
	!p 
      end (* TXT_splittable *)
    | _ -> Vec.append d w 
  in (* end of function f *)
  Vec.fold f v Vec.empty 


(* Takes care of whitespace and line breaks *)
let whitespace = "\\([ \n\t]+\\)\\|\\(''+\\)"
let whitespace_r = Str.regexp whitespace
let inline_whitespace_r = Str.regexp "[ \t]+"
let line_break_whitespace_r = Str.regexp "[ \n\t]*\n[ \n\t]*"
(* Armored xml tags *)
let xml_entity = "\\(&amp;#?[a-zA-Z0-9]+;\\)\\|\\(&#?[a-zA-Z0-9]+;\\)"
let xml_entity_r = Str.regexp xml_entity
(* All of above *)
let nobreak_r = Str.regexp (whitespace ^ "\\|" ^ xml_entity)

let lt_r = Str.regexp "<" 
let gt_r = Str.regexp ">"
(* This function produces the array of seps starting from the piece_t Vec.t, 
   taking also care of the &lt; and &gt; substitution *)
let separate_whitespace_make_seps (v: piece_t Vec.t) : sep_t Vec.t = 
  (* The function rearm re-arms the < and > tags *)
  let rearm (s: string) = 
    let s' = Str.global_replace lt_r "&lt;" s in 
    Str.global_replace gt_r "&gt;" s' 
  in 
  (* The function f is folded over v *)
  let f (d: piece_t) (sep_v: sep_t Vec.t) : sep_t Vec.t = 
    match d with 
      WS_title_start s -> Vec.append (Title_start (rearm s)) sep_v
    | WS_title_end s -> Vec.append (Title_end (rearm s)) sep_v 
    | WS_bullet s -> Vec.append (Bullet (rearm s)) sep_v
    | WS_par_break s -> Vec.append (Par_break (rearm s)) sep_v
    | WS_indent s -> Vec.append (Indent (rearm s)) sep_v
    | WS_table_line s -> Vec.append (Table_line (rearm s)) sep_v
    | WS_table_cell s -> Vec.append (Table_cell (rearm s)) sep_v
    | WS_table_caption s -> Vec.append (Table_caption (rearm s)) sep_v 
    | TXT_tag s -> Vec.append (Tag ((rearm s), 0)) sep_v
    | TXT_redirect s -> Vec.append (Redirect ((rearm s), 0)) sep_v
    | TXT_splittable s -> begin 
	(* We need to split this text into units. *)
	let l = Str.full_split nobreak_r s in 
	(* g is left-folded over l *)
	let g (vv: sep_t Vec.t) (el: Str.split_result) : sep_t Vec.t = 
	  match el with 
	    Str.Delim t -> begin
	      if Str.string_match inline_whitespace_r t 0
	      then Vec.append (Space (rearm t)) vv
	      else if Str.string_match line_break_whitespace_r t 0 
	      then Vec.append (Newline (rearm t)) vv
	      else if Str.string_match xml_entity_r t 0
	      then Vec.append (Armored_char (rearm t)) vv
	      else vv (* quotes are discarded *)
	    end
	  | Str.Text t -> Vec.append (Word ((rearm t), 0)) vv
	in List.fold_left g sep_v l 
      end
  in Vec.fold f v Vec.empty

let a_lt_r = Str.regexp "&lt;"
let a_gt_r = Str.regexp "&gt;"
(* This function splits a string respecting the Wiki markup language. *)
let split_string_preserving_markup (text: string) : sep_t Vec.t = 
  (* First, I replace &lt; and &gt; with < and >, otherwise, it's just too hard *)
  let text1  = Str.global_replace a_lt_r "<" text  in 
  let text2 = Str.global_replace a_gt_r ">" text1 in
  let text3 = remove_html_comments text2 in 
  let text' = text3 in 
  (* Now does the splitting *)
  let p0 = Vec.singleton (TXT_splittable text') in 
  let p1 = separate_string_tags p0 in 
  let p2 = separate_titles      p1 in 
  let p3 = separate_line_tags   p2 in 
  let p4 = separate_table_tags  p3 in
  separate_whitespace_make_seps p4


(* This function splits a Vec.t of strings respecting the wiki markup language *)
let split_into_words_and_seps (text_v: string Vec.t) 
    : (word array) * (sep_t array) * (int array) = 
  (* First, uses a visitor to construct a sep_t Vec.t called sep_v *)
  let vn l d r = Vec.concat (Vec.concat l (split_string_preserving_markup d)) r in 
  let sep_v' = Vec.visit_post Vec.empty vn text_v in 

  (* Removes the first \n, so that redirects are not broken. 
     This basically undoes the insertion of the first \n performed in Textbuf.cr . 
     The insertion there is useful to recognize titles, etc, that start a page. *)
  let sep_v = 
    let strip_lf (s: string) : string = 
      if String.length s > 0 
      then Str.string_after s 1
      else s
    in 
    if Vec.length sep_v' > 0 then begin 
      let first_sep = Vec.get 0 sep_v' in 
      match first_sep with 
	Par_break _ | Newline _ -> Vec.remove 0 sep_v' 
      | Title_start t    -> Vec.set 0 (Title_start    (strip_lf t)) sep_v'
      | Title_end t      -> Vec.set 0 (Title_end      (strip_lf t)) sep_v'
      | Bullet t         -> Vec.set 0 (Bullet         (strip_lf t)) sep_v'
      | Indent t         -> Vec.set 0 (Indent         (strip_lf t)) sep_v'
      | Space t          -> Vec.set 0 (Space          (strip_lf t)) sep_v'
      | Armored_char t   -> Vec.set 0 (Armored_char   (strip_lf t)) sep_v'
      | Table_line t     -> Vec.set 0 (Table_line     (strip_lf t)) sep_v'
      | Table_cell t     -> Vec.set 0 (Table_cell     (strip_lf t)) sep_v'
      | Table_caption t  -> Vec.set 0 (Table_caption  (strip_lf t)) sep_v'
      | Tag (t, i)       -> Vec.set 0 (Tag            ((strip_lf t), i)) sep_v'
      | Word (t, i)      -> Vec.set 0 (Word           ((strip_lf t), i)) sep_v'
      | Redirect (t, i)  -> Vec.set 0 (Redirect       ((strip_lf t), i)) sep_v'
    end else begin 
      sep_v' 
    end
  in 

  (* Converts sep_v to an array sep_a, fixing at the same time the word indices *)
  let n = Vec.length sep_v in 
  let sep_a = Array.make n (Title_start "") in 
  let pos = ref 0 in 
  (* The function f is iterated on sep_v *)
  let f (i: int) (s: sep_t) = 
    match s with 
      Tag (t, j) -> begin 
	sep_a.(i) <- Tag (t, !pos); 
	pos := !pos + 1
      end
    | Word (t, j) -> begin 
	sep_a.(i) <- Word (t, !pos); 
	pos := !pos + 1
      end
    | Redirect (t, j) -> begin 
	sep_a.(i) <- Redirect (t, !pos); 
	pos := !pos + 1
      end
    | _ -> sep_a.(i) <- s
  in Vec.iteri f sep_v; 
  (* Now produces the word array *)
  let word_a = Array.make !pos "" in 
  (* It produces also the array that, from each word, sends to its element in sep_a *)
  let word_sep_idx_a = Array.make !pos 0 in 
  (* The function g is iterated on sep_v *)
  let g (i: int) (s: sep_t) = 
    match s with 
      Tag (t, j) -> begin word_a.(j) <- t; word_sep_idx_a.(j) <- i end
    | Word (t, j) -> begin word_a.(j) <- t; word_sep_idx_a.(j) <- i end
    | Redirect (t, j) -> begin word_a.(j) <- t; word_sep_idx_a.(j) <- i end
    | _ -> ()
  in Array.iteri g sep_a; 
  (* All done *)
  (word_a, sep_a, word_sep_idx_a);;


(* **************************************************************** *)
(* Unit testing *)

if false then begin
  let s0 = "\n[[image:Charles Lyell.jpg|thumb|Charles Lyell]]\n[[Image:Lyell Principles frontispiece.jpg|thumb|The frontispiece from ''Principles of Geology'']]\n'''Sir Charles Lyell, 1st Baronet''', [[Order of the Thistle|KT]], ([[November 14]], [[1797]] &ndash; [[February 22]], [[1875]]), [[Scotland|Scottish]] [[lawyer]], [[geologist]], and populariser of [[Uniformitarianism (science)|uniformitarianism]].\n\nHe won the [[Copley Medal]] in 1858 and the [[Wollaston Medal]] in 1866.  After the [[Great Chicago Fire]], Lyell was one of the first to donate books to help found the [[Chicago Public Library]].\n\nUpon his death in 1875, he was buried in [[Westminster Abbey]].\n" in
  let s1 = "\n#REDIRECT [[Pollo con piselli]]\n== Titolo == \n<pre> Codice con [[markup[[ bla ]] boh]] e \n == titolo ==\n</pre>\n ==titolo2.\n=========== \n========\n == title ==" in
  let s2 = "\n{| class=\"toccolours\"  border=1 cellpadding=2 cellspacing=2 style=\"width: 700px; margin: 0 0 1em 1em; border-collapse: collapse; border: 1px solid #E2E2E2;\"\n\n|-\n! bgcolor=\"#E7EBEE\" | 1972 Debut Tour<br><br>Roy Wood's only live ELO tour.<br>After the tour, Wood, Hunt&auml;d and <br>McDowell leave ELO and form Wizzard.\n| \n* [[Roy Wood]] - [[vocals]], [[cello]], [[bass guitar]], [[guitar]], [[woodwind]]\n* [[Jeff Lynne]] - [[vocals]], [[lead guitar]], [[piano]]\n|-\n! bgcolor=\"#E7EBEE\" | 1972 - 1973 ELO 2 Tour<br><br>Bassist Mike de Albuquerque and cellist Colin Walker join ELO after the departure of Wood, Hunt, McDowell, Craig and Smith.\n| \n* [[Jeff Lynne]] - [[Vocals]], [[lead guitar]]\n* [[Bev Bevan]] - [[drums]], [[percussion]]\n|}\n" in 
  let s3 = "\n[[Adapa]] U-an ([[Berossus' ''[[Oannes]]''), " in 
  let s4 = "\n==Title of a page== \n# bullet 1\n# ''bullet2\n indent''\n{| table\n | able\n |able\n|}\n Con altra roba dopo [http://gatto.matto gatto bello] mi piace" in
  let s5 = "\n &lt;pre polla&gt; bla bla &lt;/pre &gt; &lt;blah /&gt;\n<a href=\"pollo.html\">con il pollo <pre> non</a> si fa molto </pre> di <boh /> nuovo." in
  let s6 = "\nBello ''[[link]]'' '''con''' {{stub}} e [[link]] e {{stub}} <a href=8>Mangio</a>" in 
  let s7 = "\n=== Titolo ===\nBello [[link||{{stub}} as a [[name]] long]] {{stub}} &lt;div bah=\"gog\" &gt; [[link]] </div> borom &lt;/div&gt;" in 
  let s8 = "\n==== [http://www.w3.org/TR/REC-CSS1 Cascading Style Sheets, level 1 (CSS1)], December 1996 ====\n" in 
  let s9 = "\n<pre>Sto usando tags </blah> uah <beep> con </pre> altra </beep> roba <boing> bla" in 
  let s10 = "\nInizio [[babana [[gatto ]] pollo]] [http:// [[banna]] ] testo \n<a href=\"link con </a>\">link body</a> e resto &lt;br /&gt; #redirect [[Pollo con mandorle]] del testo" in 
  let s11 = "\n==&quot;Socialism with Chinese characteristics&quot;==\n\nok\n" in
  let s12 = "\n{| style=\"background:yellow; color:green\"\n|- \n| abc || def || ghi\n|- style=\"background:red; color:white\"\n| jkl || mno || pqr\n|-\n| stu || style=\"background:silver\" | vwx || yz\n|}" in 
  let s13 = "\n{| \n| style=\"background:red; color:white\" | abc\n| def\n| bgcolor=\"red\" | &lt;font color=\"white\"&gt; ghi &lt;/font&gt;\n| jkl\n|}" in 
  let s14 = "\n<!--This is an obnoxious comment ----->{| border=\"1\"\n| &alpha;\n| align=\"center\" | cell2\n{| border=\"2\" style=\"background:#ABCDEF;\" <!-- The nested table must be on a new line -->\n| NESTED\n|-\n| TABLE\n|}\n| valign=\"bottom\" | the original table again\n| style=\"width:100px;\" |\n{| border=\"2\" style=\"background:#ABCDEF;;\"\n| A\n|}\n{| border=\"2\" style=\"background:#ABCDEF;\"\n| B || B\n|}\n| style=\"width:50px;\" |\n{| border=\"2\" style=\"background:#ABCDEF; float:left;\"\n| C\n|}\n{| border=\"2\" style=\"background:#ABCDEF; float:right;\"\n| D\n|}\n|}" in 
  let s15 = "\n{| <!--I can even put in junk here -->border=\"1\" cellpadding=\"5\" cellspacing=\"0\"\n|-\n! Column 1 || Column 2 || Column 3\n|-\n| rowspan=\"2\"| A\n| colspan=\"2\" align=\"center\"| B\n|-\n| C <!-- column 1 occupied by cell A -->\n| D \n|-\n| E\n| rowspan=\"2\" colspan=\"2\" align=\"center\"| F\n|- \n| G <!-- column 2+3 occupied by cell F -->\n|- \n| colspan=\"3\" align=\"center\"| H\n|}" in 
  let s16 = "\nThe kelvin (symbol:&nbsp;K) is the [[SI]] unit of temperature" in 
  let s17 = "\n&amp;nbsp; &lt;br&gt;gatto <br> pollo&lt;br&gt;gatto &lt;br&gt; gotto &lt;br/&gt;pollo &lt;br/&gt;pollo &lt;br&gt; pollo &lt;br/&gt;" in 

  let l = [s0; s1; s2; s3; s4; s5; s6; s7; s8; s9; s10; s11; s12; s13; s14; s15; s16; s17] in
  let e x = 
    print_seps_vec (split_string_preserving_markup x); 
    print_string "\n\n"; 
    flush stdout 
  in 
  print_string "\n\n"; 
  List.iter e l;

  let f x = 
    let x_v = Vec.singleton x in 
    let (_, sa, _) = split_into_words_and_seps x_v in 
    print_seps sa; 
    print_string "\n\n"; 
    flush stdout 
  in 
  print_string "\n\n"; 
  List.iter f l;

  print_string (remove_html_comments "babba <!--gatto--->pollo\n")

end
