(*

Copyright (c) 2007-2009 The Regents of the University of California
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

(** This analysis measures how much "text display time" each author
    has contributed.  Precisely, for each author, it computes the total of
    the number of words inserted by the author, multiplied by the number
    of seconds for which the word was present in the Wiki. *)
(** We are not making the claim that this is the best way to measure
    author contribution.  For one thing, this measure under-estimates the
    work of people who revise and reformat revisions.  Nevertheless, this
    is a possible measure, and could be even more interesting if one then
    multiplied the "text display time" for each page, by the average
    number of daily page views.  See our WikiSym 2008 paper for a more
    thorough comparison of user contribution measures. *)

type word = string 
exception Ins_text_in_deleted_chunk
exception Illegal_call_eval_newest
open Eval_defs


class page 
  (id: int)
  (title: string)
  (out_file: out_channel)
  (end_time: float) (* Time at which the wiki dump was produced. *)
  =
object (self)

  (* These are, respectively, the revision just added, and the
     immediately preceding revision. *)
  val mutable old_rev_opt : Revision.revision option = None 
  val mutable new_rev_opt  : Revision.revision option = None 

  (* Arrays of text chunks for the revision.  Chunk 0 is the live text; 
     the subsequent chunks are the portions of "dead" text. *)
  val mutable chunks_a : word array array = [| [| |] |] 
  (* For each word, we keep track of who is the author of the word
     (we remember the author id). *)
  val mutable author_a : int array array = [| [| |] |] 

  (* This is a hashtable associating to each user_id the contribution. *)
  val contributions : (int, float) Hashtbl.t = Hashtbl.create 100
  (* This is a hashtable associating to each user_id the user name. *)
  val names : (int, string) Hashtbl.t = Hashtbl.create 100

  (** This is one of the class methods that we inherit.  Normally, it does
      nothing.  We over-ride it, so that it prints the page title and id: 
      this is useful e.g. if we wanted to then multiply the contributions
      of each author to the page, by the page views of the page. *)
  method print_id_title = 
    Printf.fprintf out_file "Page: %i Title: %S\n" id title; 
    flush out_file

  (** This method increments the contribution of an author. *)
  method private inc_contribution (user_id: int) (amount: float) : unit = 
    try
      let old_amount = Hashtbl.find contributions user_id in
      Hashtbl.replace contributions user_id (old_amount +. amount)
    with Not_found -> Hashtbl.add contributions user_id amount

  (** This method analyzes the newest revision.  It first determines
      which text has been inserted new, and which has been copied,
      from the previous revision, and it then gives credit for that
      text to the author. *)
  method private eval_newest : unit = 
    (* Gets old and new revisions. Note that the method does not make
       sense unless we have both. *)
    let new_rev = match new_rev_opt with
	None -> raise Illegal_call_eval_newest
      | Some r -> r
    in 
    (* Extracts user id and name for the latest revision. 
       See revision.ml for a list of all the methods on revisions. *)
    let user_id = new_rev#get_user_id in 
    let user_name = new_rev#get_user_name in
    (* Computes the difference in time between the last and previous
       revisions. *)
    let time_delta = match old_rev_opt with
	None -> 0.
      | Some old_rev -> new_rev#get_time -. old_rev#get_time
    in 
    (* Stores the username *)
    if not (Hashtbl.mem names user_id)
    then Hashtbl.add names user_id user_name;
    (* Gets the word list of the new revision.  When the revision object
       is created, its text is automatically analyzed, so that this list
       of words has been already computed and is ready for use. *)
    let new_wl = new_rev#get_words in 
    (* Calls the function that tracks the text across revisions. *)
    let (new_chunks_a, medit_l) = Chdiff.text_tracking chunks_a new_wl in 
    (* Constructs new_author_a, which contains the authors of each
       word in the new text.  Initially, we just create chunks of the
       right size, but filled with 0. *)
    let f x = Array.create (Array.length x) 0 in 
    let new_author_a = Array.map f new_chunks_a in 
    (* Now, goes over medit_l, and fills in new_author_a properly.
       It also gives credit to each user. *)
    let rec f = function 
        Editlist.Mins (word_idx, l) -> begin 
          (* This is text added in the current version.  We mark 
	     who is the author of the text, but since the text has been
	     live for 0 time so far, we do not increase user contributions. *)
          for i = word_idx to word_idx + l - 1 do 
            new_author_a.(0).(i) <- user_id
          done
        end
      | Editlist.Mmov (src_word_idx, src_chunk_idx, 
	dst_word_idx, dst_chunk_idx, l) -> begin 
          (* This is moved text. *)
          if src_chunk_idx = 0 then begin 
	    (* If the text has been moved from chunk 0, then it has been
	       alive in the meantime, and we add credit to the user, 
	       as well as copying the information of who is the author of
	       the text. *)
            for i = 0 to l - 1 do begin 
              let a = author_a.(src_chunk_idx).(src_word_idx + i) in 
              new_author_a.(dst_chunk_idx).(dst_word_idx + i) <- a; 
              self#inc_contribution a time_delta
            end done
          end else begin 
            (* The text was not live.  Only copies the author
               information across. *)
            for i = 0 to l - 1 do 
              new_author_a.(dst_chunk_idx).(dst_word_idx + i) <-
                author_a.(src_chunk_idx).(src_word_idx + i);
            done
          end
        end
      | Editlist.Mdel (word_idx, chunk_idx, l) -> begin
	  (* If the text was live, we give credit for it. *)
	  if chunk_idx = 0 then begin
	    for i = word_idx to word_idx + l - 1 do 
	      let a = author_a.(chunk_idx).(word_idx) in
	      self#inc_contribution a time_delta
	    done
	  end
	end
    in 
    (* Does this analysis on the whole edit list. *)
    List.iter f medit_l;
    (* Replaces the old information with the new one. *)
    chunks_a <- new_chunks_a;
    author_a <- new_author_a


  (** This method outputs the results of the analysis. *)
  method private output_rep_contributions : unit = 
    (* The function f is iterated on the contributions hash table. *)
    let f (id: int) (c: float) =
      (* Gets also the user name *)
      let name = Hashtbl.find names id in
      (* Outputs the results, using days for the time measurement. *)
      Printf.fprintf out_file "Contrib by %d %S is %f\n" id name (c /. 86400.)
    in 
    Hashtbl.iter f contributions;
    (* We flush the file, so if we break, we know in which page we break. *)
    flush out_file


  (** This method is called to add a new revision to be evaluated. *)
  method add_revision 
    (id: int) (* revision id *)
    (page_id: int) (* page id *)
    (timestamp: string) (* timestamp string *)
    (time: float) (* time, as a floating point *)
    (contributor: string) (* name of the contributor *)
    (user_id: int) (* user id *)
    (ip_addr: string)
    (username: string) (* name of the user *)
    (is_minor: bool) 
    (comment: string)
    (text_init: string Vec.t) (* Text of the revision, still to be 
				 split into words *)
    : unit =
    (* Creates a new revision object.  This also takes care of parsing the
       text, etc. *)
    let r = new Revision.revision id page_id timestamp time contributor 
      user_id ip_addr username is_minor comment text_init true in 
    new_rev_opt <- Some r;
    (* Analyzes the text of the new revision and increments the
       contributions. *)
    self#eval_newest; 
    (* Copies the new revision into the old one. *)
    old_rev_opt <- new_rev_opt;

(** This method is called once the method add_revision is called for all
    revisions of a page, and is used to do any final processing.  In our case,
    we use to increase the contribution amount due to the text that is
    still live in the page. *)
  method eval: unit = 
    match old_rev_opt with 
      None -> ()  (* There were no revisions, nothing to do. *)
    | Some r -> begin
        (* Increment the contributions due to the text that is still live
	   in the page. *)
	let time_delta = end_time -. r#get_time in
	(* The function f will be iterated on the author chunks. *)
	let f (user_id: int) : unit = 
	  self#inc_contribution user_id time_delta in 
	Array.iter f author_a.(0);
        (* Outputs the results *)
        self#output_rep_contributions
      end

end (* Contribution page *)

