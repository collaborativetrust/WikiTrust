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

(** This is the revision object used for the on-line author reputation and text trust evaluation *)
class revision 
  (id: int) (* revision id *)
  (page_id: int) (* page id *)
  (timestamp: string) (* timestamp string *)
  (time: float) (* time, as a floating point *)
  (contributor: string) (* name of the contributor *)
  (user_id: int) (* user id *)
  (ip_addr: string) (* IP address *)
  (username: string) (* name of the user *)
  (is_minor: bool) 
  (comment: string)
  (text_init: string Vec.t) (* Text of the revision, still to be split into words *)
  =
  let (t, _, _, swi, s) = Text.split_into_words_seps_and_info text_init in 

  object (self)
    val words : word array = t 
    val seps  : Text.sep_t array = s
    val sep_word_idx : int array = swi 
    val is_anon : bool = (user_id = 0)

      (* Things to store: 
	 tot_edit_qual
	 n_edit_judges (these two allow the computation of the edit longevity)
	 tot_rep (total earned reputation )
         A list of revisions and authors to which the current revision gave a 
	 reputation increase, so that the same increase can be undone 
	 total_text  
	 n_text_judges

       *)


    method get_time : float = time
    method get_id : int = id
    method get_ip : string = ip_addr 
    method get_page_id : int = page_id
    method get_user_id : int = user_id
    method get_user_name : string = 
      if is_anon then
        ip_addr
      else
        username
    method get_is_anon : bool = is_anon

    method get_words : word array = words
    method get_n_words : int = Array.length words
    method print_words : unit = begin Text.print_words words ; print_newline () end

    method get_seps : Text.sep_t array = seps 
    method get_sep_word_idx : int array = sep_word_idx 

    (* This is an array of word reputations *)
    val mutable word_trust : float array = [| |] 
    method set_word_trust (a: float array) : unit = word_trust <- a
    method get_word_trust : float array = word_trust

    (* This is an array of word origins *)
    val mutable word_origin : int array = [| |] 
    method set_word_origin (a: int array) : unit = word_origin <- a
    method get_word_origin : int array = word_origin

    method print_words_and_seps : unit = begin 
      Text.print_words_and_seps words seps;
      print_newline (); 
    end

    method output_rev_preamble trust_file = 
      (* prints standard stuff *)
      Printf.fprintf trust_file "<revision>\n<id>%d</id>\n" id; 
      Printf.fprintf trust_file "<timestamp>%s</timestamp>\n" timestamp;
      Printf.fprintf trust_file "<contributor>\n%s</contributor>\n" contributor;
      if is_minor then Printf.fprintf trust_file "<minor />\n";
      Printf.fprintf trust_file "<comment>%s</comment>\n" comment;
      Printf.fprintf trust_file "<text xml:space=\"preserve\">"

    method output_revision trust_file = 
      (** This method is used to output the revision to an output file. *)
      self#output_rev_preamble trust_file; 
      (* This function f is iterated on the array *)
      let f (s: Text.sep_t) : unit = 
        match s with 
	  Text.Title_start (t, _) | Text.Title_end (t, _) | Text.Par_break t
        | Text.Bullet (t, _) | Text.Indent (t, _) | Text.Space t | Text.Newline t
	| Text.Armored_char (t, _) | Text.Table_line (t, _) | Text.Table_cell (t, _)
	| Text.Table_caption (t, _) 
        | Text.Tag (t, _) | Text.Redirect (t, _) | Text.Word (t, _) -> 
	    output_string trust_file t
      in
      Array.iter f seps;
      output_string trust_file "</text>\n</revision>\n"


    method output_rev_text (include_origin: bool) trust_file = 
      (** This method is used to output the colorized version of a 
          revision to an output file. *)
      self#output_rev_preamble trust_file; 
      (* Now we must write the text of the revision *)
      let b = Buffer.create 1000 in 
      produce_annotated_markup seps word_trust word_origin false include_origin b; 
      Buffer.output_buffer trust_file b; 
      output_string trust_file "</text>\n</revision>\n"


    method output_trust_revision = self#output_rev_text false

    method output_trust_origin_revision = self#output_rev_text true 

  end (* revision object *)
