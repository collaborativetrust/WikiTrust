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

(** chdiff.ml : here we compare chunks of text, to trace text through
    revisions, and to compute edit distances *)


type edit = Ins of int * int         (* Ins i l means add l words at position i *)
	    | Del of int * int       (* Del i l means delete l words from position i *)
	    | Mov of int * int * int (* Mov i j l means move l words from pos i to pos l *)

(* same as edit, but for the case when the lhs and rhs are lists of chunks *)
type medit = Mins of int * int * int (* Mins i k l is insert l chars at pos i of chunk k *)
	     | Mdel of int * int * int (* Mdel i k l is del l chars at pos i of chunk k *)
	     | Mmov of int * int * int * int * int (* Mmov i k j n l is mov l chars from pos i of chunk k to pos j of chunk n *)

type word = string (* so that we know what is going on *)
type heap_el = int * int * int    (* (len, pos1, pos2) *)
type index_t = ((word * word * word), int) Hashtbl_bounded.t

(** This is the maximum number of matches for a word pair that we
    track. If a word pair has more than this number of matches, we
    disregard them all, as we classify the word pair as not sufficiently
    distinctive. *)
let max_matches = 40 
(** This is the minimum length of any dead chunk we keep around.  Dead chunks shorter
    than this are not kept, with the idea that short amounts of text are not 
    rescued, but simply, freshly rewritten. *)
let min_dead_chunk_len = 6
(** This is the minimum length of a match between existing and old text for considering 
    copying to have happened.  It must be 2 <= min_copy_amount <= min_dead_chunk_len *)
let min_copy_amount = 2
(** Minimum amount that can be considered copied from dead chunks.  It must satisfy 
    min_copy_amount <= min_dead_copy_amount <= min_dead_chunk_len, and it is sensible
    to take min_dead_copy_amount = min_dead_chunk_len *)
let min_dead_copy_amount = 6

(** These are used to delimit chunks when the chunks are put all together in the 
    zipping calculations. Do NOT remove the spaces. *)
let left_separator = " 1 "
let right_separator = " 2 "



module Heap = Coda.PriorityQueue

(* Quality functions for matches *)

let quality (l: int) (i1: int) (len1: int) (i2: int) (len2: int) : float = 
  let l' = float_of_int l in 
  let i1' = float_of_int i1 in 
  let len1' = float_of_int len1 in 
  let i2' = float_of_int i2 in 
  let len2' = float_of_int len2 in 
  l' *. (1.0 -. (0.3 *. abs_float ((i1' /. len1') -. (i2' /. len2'))))

let quality_survival (l: int) (i1: int) (len1: int) (i2: int) (len2: int) (is_dead: bool)
    : float = 
  let l' = float_of_int l in 
  let i1' = float_of_int i1 in 
  let len1' = float_of_int len1 in 
  let i2' = float_of_int i2 in 
  let len2' = float_of_int len2 in 
  if is_dead then 
    l' *. 0.6 
  else
    l' *. (1.0 -. (0.3 *. abs_float ((i1' /. len1') -. (i2' /. len2'))))

(** Useful for debugging purposes *)
let rec print_diff l = 
  match l with 
    d :: l' ->
      begin
	begin 
	  match d with 
	    Ins (i, l) -> Printf.printf "\nIns %d %d" i l 
	  | Del (i, l) -> Printf.printf "\nDel %d %d" i l 
	  | Mov (i, j, l) -> Printf.printf "\nMov %d %d %d" i j l 
	end;
	print_diff l'
      end
  | [] -> Printf.printf "\n";;

let rec print_mdiff l = 
  match l with 
    d :: l' ->
      begin
	begin 
	  match d with 
	    Mins (i, k, l) -> Printf.printf "\nIns (%d, %d) %d" i k l 
	  | Mdel (i, k, l) -> Printf.printf "\nDel (%d, %d) %d" i k l 
	  | Mmov (i, k, j, n, l) -> Printf.printf "\nMov (%d, %d) (%d, %d) %d" i k j n l 
	end;
	print_mdiff l'
      end
  | [] -> Printf.printf "\n";;

let print_chunks (waa: word array array) = Array.iter Text.print_words waa;;

(* **************************************************************** *)
(* Edit distance computation *)

let edit_distance (edits: edit list) (l: int) : float = 
  let tot_ins = ref 0 in 
  let tot_del = ref 0 in 
  (* Adds up Ins and Del, and leaves the Mov for later analysis *)
  let rec sum_changes (el: edit list) : (int * int * int) list =  
    match el with 
      [] -> []
    | e :: l -> begin
	match e with 
	  Mov (i, j, m) -> (i, j, m) :: (sum_changes l)
	| Ins (i, len) -> begin
	    tot_ins := !tot_ins + len; 
	    sum_changes l
	  end
	| Del (i, len) -> begin
	    tot_del := !tot_del + len; 
	    sum_changes l
	  end
      end
  in
  let mov_l = sum_changes edits in 
  (* Computes the contribution of movs *)
  (* Makes an array of the moves *)
  let a = Array.of_list mov_l in 
  (* comparison for sorting *)
  let cmp m1 m2 = 
    let (i1, j1, l1) = m1 in 
    let (i2, j2, l2) = m2 in 
    let d = i1 - i2 in 
    if d > 0 then 1
    else if d < 0 then -1 
    else 0
  in
  (* sorts the array *)
  Array.sort cmp a;
  (* now we sort it wrt the move destination, 
     adding contributions as we go along *)
  let tot_mov = ref 0 in 
  (* sorts between lower_b and upper_b *)
  let lower_b = ref 0 in 
  let upper_b = ref ((Array.length a) - 1) in 
  while !upper_b > !lower_b do 
    begin 
      (* first, we go up *)
      let change = ref 0 in 
      for i = !lower_b to !upper_b - 1 do 
	begin
	  let (i1, j1, l1) = a.(i) in 
	  let (i2, j2, l2) = a.(i+1) in 
	  if j2 < j1 then 
	    begin
	      (* swaps, and takes cost into consideration *)
	      let m = a.(i) in 
	      a.(i) <- a.(i+1);
	      a.(i+1) <- m; 
	      tot_mov := !tot_mov + l1 * l2; 
	      (* keeps track of the upper change in sort order *)
	      change := i
	    end
	end
      done; 
      upper_b := !change; 
      (* then we go down *)
      change := !upper_b; 
      for i = !upper_b downto !lower_b + 1 do 
	begin 
	  let (i2, j2, l2) = a.(i) in 
	  let (i1, j1, l1) = a.(i-1) in 
	  if j2 < j1 then 
	    begin
	      (* swaps, and takes cost into consideration *)
	      let m = a.(i) in 
	      a.(i) <- a.(i-1);
	      a.(i-1) <- m; 
	      tot_mov := !tot_mov + l1 * l2; 
	      (* keeps track of the upper change in sort order *)
	      change := i
	    end
	end
      done; 
      lower_b := !change; 
    end
  done;
  (* computes the distance *)
  let ins' = float_of_int !tot_ins in 
  let del' = float_of_int !tot_del in 
  let mov' = float_of_int !tot_mov in 
  let len' = (if l = 0 then 1.0 else float_of_int l) in 
  (max ins' del') -. (0.5 *. (min ins' del')) +. (mov' /. len');;
	    

(* **************************************************************** *)
(* Edit difference computation *)

(** This function creates an index as needed for words2 in edit_diff *)
let make_index_diff (words: word array) : index_t = 
  let len = Array.length words in 
  let idx = Hashtbl_bounded.create (1 + len) (10 * max_matches) in 
  (* fills up the index *)
  for i = 0 to len - 3 do 
    let word_tuple = (words.(i), words.(i + 1), words.(i + 2)) in 
    Hashtbl_bounded.add idx word_tuple i
  done; 
  idx;;


(** This function computes a list of edit commands that enable to go from 
    words1 to words2 *)
let edit_diff 
    (words1: word array) 
    (words2: word array) 
    (index2: index_t) : edit list 
    =

  (* creates the heap for the matches *)
  let heap = Heap.create () in 
  
  let len1 = Array.length (words1) in 
  let len2 = Array.length (words2) in 
  (* prev_matches is used to avoid putting smaller matches in the heap when a bigger 
     one in the same location has already been found *)
  let prev_matches = ref [] in 
  (* the - 2 is because we use word pairs to index the hash table *)
  for i1 = 0 to len1 - 3 do 
    let word_tuple = (words1.(i1), words1.(i1 + 1), words1.(i1 + 2)) in 
    if Hashtbl_bounded.mem index2 word_tuple then
      begin
	let matches = Hashtbl_bounded.find_all index2 word_tuple in 
	(* We care only if there are at most max_matches matches. 
	   If there are too many matches, removes them all. *)
	if List.length (matches) > max_matches 
	then Hashtbl_bounded.remove_all index2 word_tuple
	else
	  begin
	    (* This function processes a match at position i2 of words2 *)
	    let process_match (i2: int) = 
	      (* if i2 - 1 is in prev_matches, then we don't need to do anything, 
		 since (i1 - 1, i2 - 1) has already been counted as a match, and is longer *)
	      if not (List.mem (i2 - 1) !prev_matches) then
		begin
		  (* Now we check how long the match is; the min len is 3 of course,
		     since the index contains word triples *)
		  let l = ref 3 in 
		  while (i1 + !l < len1) && (i2 + !l < len2) && 
		    (words1.(i1 + !l) = words2.(i2 + !l)) do l := !l + 1 done; 
		  (* Ok, the match starts in i1, i2, and has length !l *)
		  let q = quality !l i1 len1 i2 len2 in 
		  (* Adds it to the heap *)
		  ignore (Heap.add heap (!l, i1, i2) q)
		end
		  (* Applies it to all matches *)
	    in List.iter process_match matches; 
	    (* keeps track of the previous matches *)
	    prev_matches := matches
	  end
      end
    else prev_matches := []
  done;
  
  (* At this point, all the matches between words1 and words2 are in heap *)
  
  (* Creates two arrays, which keep track of what has been matched.
     Remember, here we allow only single matches on either side. *)
  let matched1 = Array.make len1 0 in 
  let matched2 = Array.make len2 0 in
  (* This is for quickly identifying submatches *)
  let match_id = ref 0 in 
  (* diff is where we store the result *)
  let diff = ref [] in 

  (* Removes the matches one by one *)
  while not (Heap.is_empty heap) do 
    begin
      let m = Heap.take heap in
      match_id := !match_id + 1; 
      let (l, i1, i2) = m.Heap.contents in 
      (* Checks whether it has been matched.  As we pull out 
	 longest matches first (in each chunk), we can just
	 check the endpoints. *)
      if (matched1.(i1) = 0 && matched2.(i2) = 0) then 
	(* lower end is not matched *)
	if (matched1.(i1 + l - 1) = 0 && matched2.(i2 + l - 1) = 0) then 
	  (* upper end is not matched *)
	  begin
	    (* adds the edit command *)
	    diff := Mov (i1, i2, l) :: !diff; 
	    (* ... and marks it matched *)
	    for l' = 0 to l - 1 do begin
	      matched1.(i1 + l') <- !match_id; 
	      matched2.(i2 + l') <- !match_id
	    end
	    done
	  end
	else 
	  (* lower end is unmatched, upper end is matched *)
	  begin
	    (* Figures out the longest residual match, and puts it back into the heap *)
	    let k = ref (l - 2) in (* we know l - 1 is matched already *)
	    while (matched1.(i1 + !k) <> 0 || matched2.(i2 + !k) <> 0) do k := !k - 1 done; 
	    let res_l = !k + 1 in (* res_l is the len of the residual match *)
	    if res_l > 1 then begin
	      let q = quality res_l i1 len1 i2 len2 in 
	      ignore (Heap.add heap (res_l, i1, i2) q)
	    end
	  end
      else
	(* lower end is matched *)
	if (matched1.(i1 + l - 1) = 0 && matched2.(i2 + l - 1) = 0) then 
	  (* upper end is not matched *)
	  begin
	    (* Figures out the longest residual match, and puts it back into the heap *)
	    let j = ref 1 in (* we know 0 is matched already *)
	    while (matched1.(i1 + !j) <> 0 || matched2.(i2 + !j) <> 0) do j := !j + 1 done; 
	    let res_l = l - !j in (* res_l is the len of the residual match *)
	    if res_l > 1 then begin
	      let q = quality res_l (i1 + !j) len1 (i2 + !j) len2 in 
	      ignore (Heap.add heap (res_l, i1 + !j, i2 + !j) q)
	    end
	  end
	else 
	  (* both ends are matched *)
	  (* we need to look at whether there is an umatched portion
	     in the middle only if both ends are not matched by the
	     same match_id.  Note that there can be at most one
	     unmatched portion, as we pull out matches from the heap
	     longest first *)
	  if (matched1.(i1) <> matched1.(i1 + l - 1)) && 
	     (matched2.(i2) <> matched2.(i2 + l - 1)) then 
	       begin 
		 let j = ref 1 in 
		 while (!j < l - 1 && (matched1.(i1 + !j) <> 0 || matched2.(i2 + !j) <> 0)) do 
		   j := !j + 1 done;
		 let k = ref (!j + 1) in 
		 while (!k < l - 1 && not (matched1.(i1 + !k) <> 0 || matched2.(i2 + !k) <> 0)) do 
		   k := !k + 1 done;
		 let res_l = !k - !j in 
		 if res_l > 1 then begin
		   let q = quality res_l (i1 + !j) len1 (i2 + !j) len2 in 
		   ignore (Heap.add heap (res_l, i1 + !j, i2 + !j) q)
		 end
	       end
    end
  done; (* while heap is not empty *)
  
  (* Great; at this point, we have to cope with the unmatched stuff *)
  (* first from words1 *)
  let in_matched = ref true in 
  let unmatched_start = ref 0 in 
  for i1 = 0 to len1 - 1 do 
    if (!in_matched) && (matched1.(i1) = 0) then 
      begin 
	(* this is the start of an unmatched chunk *)
	in_matched := false;
	unmatched_start := i1; 
      end;
    if (not !in_matched) && (matched1.(i1) <> 0) then 
      begin
	(* this is the end of an unmatched chunk *)
	in_matched := true;
	if i1 > !unmatched_start then
	  diff := Del (!unmatched_start, i1 - !unmatched_start) :: !diff
      end
  done; 
  (* takes care of the last portion *)
  if (not !in_matched) && len1 > !unmatched_start then 
    diff := Del (!unmatched_start, len1 - !unmatched_start) :: !diff; 

  (* then from words2 *)
  let in_matched = ref true in 
  let unmatched_start = ref 0 in 
  for i2 = 0 to len2 - 1 do 
    if (!in_matched) && (matched2.(i2) = 0) then 
      begin 
	(* this is the start of an unmatched chunk *)
	in_matched := false;
	unmatched_start := i2; 
      end;
    if (not !in_matched) && (matched2.(i2) <> 0) then 
      begin
	(* this is the end of an unmatched chunk *)
	in_matched := true;
	if i2 > !unmatched_start then
	  diff := Ins (!unmatched_start, i2 - !unmatched_start) :: !diff
      end
  done; 
  (* takes care of the last portion *)
  if (not !in_matched) && len2 > !unmatched_start then 
    diff := Ins (!unmatched_start, len2 - !unmatched_start) :: !diff; 

  (* all done *)
  !diff;;


(* **************************************************************** *)
(* Text survival and tracking *)

(* We index by pairs *)
let make_survival_index (chunks: word array array) : ((word * word), (int * int)) Hashtbl_bounded.t = 
  (* makes a hashtable of the right size *)
  let f t a = t + (Array.length a) in 
  let tot_els = Array.fold_left f 0 chunks in 
  let idx = Hashtbl_bounded.create (1 + tot_els) (10 * max_matches) in 
  (* now fills up the index *)
  let num_chunks = Array.length chunks in 
  (* makes an index for all chunks *)
  for c_idx = 0 to num_chunks - 1 do 
    begin
      let c = chunks.(c_idx) in 
      let chunk_len = Array.length (c) in 
      for i = 0 to chunk_len - 2 do 
	let word_pair = (c.(i), c.(i+1)) in 
	Hashtbl_bounded.add idx word_pair (c_idx, i)
      done
    end
  done;
  idx;;
	

(** [text_survival words1 words1_attr dead_chunks dead_chunks_attr words2] 
    takes a piece of text [words1], where each word has its own attribute,
    specified in [words1_attr], and a list of dead chunks [dead_chunks] with 
    their attributes, in [dead_chunks_attr], and a list of new words [words2],
    representing the successive version of the text.  
    The function matches [words2] with [words1] and [dead_chunks], producing 
    [dead_chunks_2].  It also creates attributes for [words2] and 
    [dead_chunks_2], as follows: 
    - the function [f_inherit live1 live2] specifies how to compute an attribute of 
      version 2 ([words2] or [dead_chunks_2]) from one of version 1
      ([words1] or [dead_chunks]), given two flags [live1] and [live2] that 
      specify whether the text is live or dead in versions 1 and 2. 
    - the constant [c_new] specifies an attribute of new text
      for [words2]. 

    It also takes a function attr_inherit, which specifies how attributes 
    are modified when inherited from one revision to the next
 *)
let text_survival 
    (words1: word array)
    (words1_attr: 'a array) 
    (dead_chunks: word array list)
    (dead_chunks_attr: 'a array list)
    (words2: word array)
    (f_inherit: bool -> bool -> 'a -> 'a)
    (c_new: 'a)
    : ('a array) * (word array list) * ('a array list) 
    =
  let chunks1_list = words1 :: dead_chunks in 
  let attr1_list = words1_attr :: dead_chunks_attr in 
  let chunks1 = Array.of_list chunks1_list in 
  let attr1 = Array.of_list attr1_list in 

  (* Make an index of chunks1 *)
  let idx1 = make_survival_index chunks1 in 
  
  (* creates the heap for the matches *)
  let heap = Heap.create () in 
  
  let len2 = Array.length (words2) in 
  let len1 = Array.map Array.length chunks1 in 

  (* This function tells us whether a match is big enough to be worth 
     putting in the heap, depending whether it's a match with live or dead 
     text (given by c1_idx = 0). *)
  let big_enough l c1_idx = 
    if c1_idx = 0 then l >= min_copy_amount 
    else l >= min_dead_copy_amount
  in

    (* prev_matches is used to avoid putting smaller matches in the heap when a bigger 
     one in the same location has already been found *)
  let prev_matches = ref [] in 
  (* the - 2 is because we use word pairs to index the hash table *)
  for i2 = 0 to len2 - 2 do 
    let word_tuple = (words2.(i2), words2.(i2 + 1)) in 
    if Hashtbl_bounded.mem idx1 word_tuple then
      begin
	let matches = Hashtbl_bounded.find_all idx1 word_tuple in 
	(* We care only if there are at most max_matches matches *)
	if List.length (matches) > max_matches 
	then Hashtbl_bounded.remove_all idx1 word_tuple
	else 
	  begin
	    (* This function processes a match at position i2 of words2 *)
	    let process_match (m: int * int) = 
	      let (c1_idx, i1) = m in 
	      (* if (c1_idx, i1 - 1) is in prev_matches, then we don't need to do anything, 
		 since a superset match has already been added *)
	      if not (List.mem (c1_idx, i1 - 1) !prev_matches) then
		begin
		  (* Now we check how long the match is; the min len is 2 of course,
		     since the index contains word pairs *)
		  let l = ref 2 in 
		  while (i1 + !l < len1.(c1_idx)) && (i2 + !l < len2) && 
		    (chunks1.(c1_idx).(i1 + !l) = words2.(i2 + !l)) do l := !l + 1 done; 
		  (* Ok, the match starts in i1, i2, and has length !l *)
		  if big_enough !l c1_idx then 
		    (* we add it only if it is long enough *)
		    begin
		      let q = quality_survival !l i1 len1.(c1_idx) i2 len2 (c1_idx > 0) in 
		      (* Adds it to the heap *)
		      ignore (Heap.add heap (!l, c1_idx, i1, i2) q)
		    end
		end
		  (* Applies it to all matches *)
	    in List.iter process_match matches; 
	    (* keeps track of the previous matches *)
	    prev_matches := matches
	  end
      end
    else prev_matches := []
  done;

  (* At this point, all the matches between chunks1 and words2 are in heap *)
  (* Creates an array which keep track of what has been matched in words2 *)
  let matched2 = Array.make len2 0 in
  (* This is used to quickly find out if both ends of a supposed match are 
     matched by the same match *)
  let match_id = ref 0 in 
  (* matched1 keeps track instead of how many times a piece of text has been matched. *)
  let f a = Array.make (Array.length a) 0 in 
  let matched1 = Array.map f chunks1 in 
  (* words2_attr are the new attributes of words in words2 *)
  let words2_attr = Array.make len2 c_new in 

  (* Removes the matches one by one *)
  while not (Heap.is_empty heap) do 
    begin
      let m = Heap.take heap in
      match_id := !match_id + 1; 
      let (l, c1_idx, i1, i2) = m.Heap.contents in 
      (* Checks whether it has been matched.  As we pull out 
	 longest matches first (in each chunk), we can just
	 check the endpoints. *)
      if matched2.(i2) = 0 then 
	(* lower end is not matched *)
	if matched2.(i2 + l - 1) = 0 then 
	  (* upper end is not matched *)
	  for l' = 0 to l - 1 do 
	    begin
	      (* computes the new attribute of the text *)
	      words2_attr.(i2 + l') <- f_inherit (c1_idx = 0) true attr1.(c1_idx).(i1 + l'); 
	      (* ... and marks the text matched *)
	      matched2.(i2 + l') <- !match_id;
	      matched1.(c1_idx).(i1 + l') <- 1 + matched1.(c1_idx).(i1 + l')
	    end
	  done
	else 
	  (* lower end is unmatched, upper end is matched *)
	  begin
	    (* Figures out the longest residual match, and puts it back into the heap *)
	    let k = ref (l - 2) in (* we know l - 1 is matched already *)
	    while matched2.(i2 + !k) <> 0 do k := !k - 1 done; 
	    let res_l = !k + 1 in (* res_l is the len of the residual match *)
	    if big_enough res_l c1_idx then begin
	      let q = quality_survival res_l i1 len1.(c1_idx) i2 len2 (c1_idx > 0) in 
	      ignore (Heap.add heap (res_l, c1_idx, i1, i2) q)
	    end
	  end
      else
	(* lower end is matched *)
	if matched2.(i2 + l - 1) = 0 then 
	  (* upper end is not matched *)
	  begin
	    (* Figures out the longest residual match, and puts it back into the heap *)
	    let j = ref 1 in (* we know 0 is matched already *)
	    while matched2.(i2 + !j) <> 0 do j := !j + 1 done; 
	    let res_l = l - !j in (* res_l is the len of the residual match *)
	    if big_enough res_l c1_idx then begin
	      let q = quality_survival 
		res_l (i1 + !j) len1.(c1_idx) (i2 + !j) len2 (c1_idx > 0) in 
	      ignore (Heap.add heap (res_l, c1_idx, i1 + !j, i2 + !j) q)
	    end
	  end
	else 
	  (* both ends are matched *)
	  (* we need to look at whether there is an umatched portion in the middle, 
	     if the match ends have different ids *)
	  if matched2.(i2) <> matched2.(i2 + l - 1) then 
	    (* there can be only one such portion, as we pull out matches from the heap
	       longest first *)
	    begin 
	      let j = ref 1 in 
	      while !j < l - 1 && matched2.(i2 + !j) <> 0 do 
		j := !j + 1 done;
	      let k = ref (!j + 1) in 
	      while !k < l - 1 && matched2.(i2 + !k) <> 0 do 
		k := !k + 1 done;
	      let res_l = !k - !j in 
	      if big_enough res_l c1_idx then begin
		let q = quality_survival 
		  res_l (i1 + !j) len1.(c1_idx) (i2 + !j) len2 (c1_idx > 0) in 
		ignore (Heap.add heap (res_l, c1_idx, i1 + !j, i2 + !j) q)
	      end
	    end
    end
  done; (* while heap is not empty *)

  (* I removed the surround effect; it is now replaced by min_copy_amount *)

  (* Great; at this point, we have to cope with the unmatched stuff on the 
     first side, to generate the dead chunks for the second version. *)
  let dead_chunks2_l = ref [] in 
  let dead_chunks2_attr_l = ref [] in 

  for c1_idx = 0 to Array.length (matched1) - 1 do 
    begin 
      let in_matched = ref true in 
      let unmatched_start = ref 0 in 
      for i1 = 0 to len1.(c1_idx) - 1 do 
	if (!in_matched) && (matched1.(c1_idx).(i1) = 0) then 
	  begin 
	    (* this is the start of an unmatched chunk *)
	    in_matched := false;
	    unmatched_start := i1; 
	  end;
	if (not !in_matched) && (matched1.(c1_idx).(i1) > 0) then 
	  begin
	    (* this is the end of an unmatched chunk *)
	    in_matched := true;
	    if i1 - !unmatched_start >= min_dead_chunk_len then
	      begin
		(* Creates the word array for the dead chunk *)
		let l = i1 - !unmatched_start in 
		dead_chunks2_l := (Array.sub chunks1.(c1_idx) !unmatched_start l) 
		                  :: !dead_chunks2_l;
		(* Creates the attribute array for the dead chunk *)
		let dead_chunk2_attr = Array.sub attr1.(c1_idx) !unmatched_start l in 
		(* Now recomputes the attributes *)
		for k = 0 to l - 1 do 
		  dead_chunk2_attr.(k) <- f_inherit (c1_idx = 0) false dead_chunk2_attr.(k)
		done;
		(* and adds it to the list of results *)
		dead_chunks2_attr_l := dead_chunk2_attr :: !dead_chunks2_attr_l
	      end
	  end
      done; 
      (* takes care of the last portion *)
      if (not !in_matched) && len1.(c1_idx) >= (!unmatched_start + min_dead_chunk_len) then 
	begin
	  (* Creates the word array for the dead chunk *)
	  let l = len1.(c1_idx) - !unmatched_start in 
	  dead_chunks2_l := (Array.sub chunks1.(c1_idx) !unmatched_start l) 
	                    :: !dead_chunks2_l;
	  (* Creates the attribute array for the dead chunk *)
	  let dead_chunk2_attr = Array.sub attr1.(c1_idx) !unmatched_start l in 
	  (* Now recomputes the attributes *)
	  for k = 0 to l - 1 do 
	    dead_chunk2_attr.(k) <- f_inherit (c1_idx = 0) false dead_chunk2_attr.(k)
	  done;
	  (* and adds it to the list of results *)
	  dead_chunks2_attr_l := dead_chunk2_attr :: !dead_chunks2_attr_l
	end
    end
  done; (* for each c1_idx *)

  (* We can finally assemble the result *)
  (words2_attr, !dead_chunks2_l, !dead_chunks2_attr_l);;

(* **************************************************************** *)
(* New version of code *)

(** [text_tracking chunks1 words2] takes an array of text [chunks1], 
    and a new word [words2], and produces a new list of chunks [chunks2], 
    and a list of matches that pair up the new text with the old one. 
    chunks2.(0) is guaranteed to exist (but it might be empty). *)

let text_tracking
    (chunks1: word array array)
    (words2: word array)
    : ((word array array) * (medit list))
    =

  (* Make an index of chunks1 *)
  let idx1 = make_survival_index chunks1 in 
  
  (* creates the heap for the matches *)
  let heap = Heap.create () in 
  
  let len2 = Array.length (words2) in 
  let len1 = Array.map Array.length chunks1 in 

  (* This function tells us whether a match is big enough to be worth 
     putting in the heap, depending whether it's a match with live or dead 
     text (given by c1_idx = 0). *)
  let big_enough l c1_idx = 
    if c1_idx = 0 then l >= min_copy_amount 
    else l >= min_dead_copy_amount
  in

    (* prev_matches is used to avoid putting smaller matches in the heap when a bigger 
     one in the same location has already been found *)
  let prev_matches = ref [] in 
  (* the - 2 is because we use word pairs to index the hash table *)
  for i2 = 0 to len2 - 2 do 
    let word_tuple = (words2.(i2), words2.(i2 + 1)) in 
    if Hashtbl_bounded.mem idx1 word_tuple then
      begin
	let matches = Hashtbl_bounded.find_all idx1 word_tuple in 
	(* We care only if there are at most max_matches matches *)
	if List.length (matches) > max_matches 
	then Hashtbl_bounded.remove_all idx1 word_tuple
	else 
	  begin
	    (* This function processes a match at position i2 of words2 *)
	    let process_match (m: int * int) = 
	      let (c1_idx, i1) = m in 
	      (* if (c1_idx, i1 - 1) is in prev_matches, then we don't need to do anything, 
		 since a superset match has already been added *)
	      if not (List.mem (c1_idx, i1 - 1) !prev_matches) then
		begin
		  (* Now we check how long the match is; the min len is 2 of course,
		     since the index contains word pairs *)
		  let l = ref 2 in 
		  while (i1 + !l < len1.(c1_idx)) && (i2 + !l < len2) && 
		    (chunks1.(c1_idx).(i1 + !l) = words2.(i2 + !l)) do l := !l + 1 done; 
		  (* Ok, the match starts in i1, i2, and has length !l *)
		  if big_enough !l c1_idx then 
		    (* we add it only if it is long enough *)
		    begin
		      let q = quality_survival !l i1 len1.(c1_idx) i2 len2 (c1_idx > 0) in 
		      (* Adds it to the heap *)
		      ignore (Heap.add heap (!l, c1_idx, i1, i2) q)
		    end
		end
		  (* Applies it to all matches *)
	    in List.iter process_match matches; 
	    (* keeps track of the previous matches *)
	    prev_matches := matches
	  end
      end
    else prev_matches := []
  done;

  (* At this point, all the matches between chunks1 and words2 are in heap. *)
  (* We now produce the true list of maximal matches, non-overlapping in words2, 
     that explains the inheritance of text. *)

  (* Creates an array which keeps track of what has been matched in words2 *)
  let matched2 = Array.make len2 0 in
  (* This is used to quickly find out if both ends of a supposed match are 
     matched by the same match *)
  let match_id = ref 0 in 
  (* matched1 keeps track instead of how many times a piece of text has been matched. *)
  let f a = Array.make (Array.length a) 0 in 
  let matched1 = Array.map f chunks1 in 
  (* diff is where we store the edit list *)
  let diff = ref [] in 
  (* Removes the matches one by one *)
  while not (Heap.is_empty heap) do 
    begin
      let m = Heap.take heap in
      match_id := !match_id + 1; 
      let (l, c1_idx, i1, i2) = m.Heap.contents in 
      (* Checks whether it has been matched.  As we pull out 
	 longest matches first (in each chunk), we can just
	 check the endpoints. *)
      if matched2.(i2) = 0 then 
	(* lower end is not matched *)
	if matched2.(i2 + l - 1) = 0 then begin 
	  (* upper end is not matched *)
	  diff := Mmov (i1, c1_idx, i2, 0, l) :: !diff; 
	  for l' = 0 to l - 1 do begin
	      (* marks the text matched *)
	      matched2.(i2 + l') <- !match_id;
	      matched1.(c1_idx).(i1 + l') <- 1 + matched1.(c1_idx).(i1 + l')
	  end done 
	end
	else begin 
	  (* lower end is unmatched, upper end is matched *)
	  (* Figures out the longest residual match, and puts it back into the heap *)
	  let k = ref (l - 2) in (* we know l - 1 is matched already *)
	  while matched2.(i2 + !k) <> 0 do k := !k - 1 done; 
	  let res_l = !k + 1 in (* res_l is the len of the residual match *)
	  if big_enough res_l c1_idx then begin
	    let q = quality_survival res_l i1 len1.(c1_idx) i2 len2 (c1_idx > 0) in 
	    ignore (Heap.add heap (res_l, c1_idx, i1, i2) q)
	  end
	end
      else
	(* lower end is matched *)
	if matched2.(i2 + l - 1) = 0 then 
	  (* upper end is not matched *)
	  begin
	    (* Figures out the longest residual match, and puts it back into the heap *)
	    let j = ref 1 in (* we know 0 is matched already *)
	    while matched2.(i2 + !j) <> 0 do j := !j + 1 done; 
	    let res_l = l - !j in (* res_l is the len of the residual match *)
	    if big_enough res_l c1_idx then begin
	      let q = quality_survival 
		res_l (i1 + !j) len1.(c1_idx) (i2 + !j) len2 (c1_idx > 0) in 
	      ignore (Heap.add heap (res_l, c1_idx, i1 + !j, i2 + !j) q)
	    end
	  end
	else 
	  (* both ends are matched *)
	  (* we need to look at whether there is an umatched portion in the middle, 
	     if the match ends have different ids *)
	  if matched2.(i2) <> matched2.(i2 + l - 1) then 
	    (* there can be only one such portion, as we pull out matches from the heap
	       longest first *)
	    begin 
	      let j = ref 1 in 
	      while !j < l - 1 && matched2.(i2 + !j) <> 0 do 
		j := !j + 1 done;
	      let k = ref (!j + 1) in 
	      while !k < l - 1 && matched2.(i2 + !k) <> 0 do 
		k := !k + 1 done;
	      let res_l = !k - !j in 
	      if big_enough res_l c1_idx then begin
		let q = quality_survival 
		  res_l (i1 + !j) len1.(c1_idx) (i2 + !j) len2 (c1_idx > 0) in 
		ignore (Heap.add heap (res_l, c1_idx, i1 + !j, i2 + !j) q)
	      end
	    end
    end
  done; (* while heap is not empty *)

  (* Now, this is very counter-intuitive, but there is a slight chance that the list !diff contains 
     [... Mmov (match1); Mmov (match2); ...] with match2 entirely contained in match1 -- even though
     the biggest matches are supposed to be found first, and thus end up later in !diff. 
     This would create problems later on in Wikipage, since !diff is then scanned linearly to attribute 
     text: it is safer if match1, the bigger match, is processed last, in case it is bigger. 
     Note that if match2 is bigger, it is not possible that match1, found later, is entirely contained 
     in match2, due to how the above algorithm is written. *)
  diff := List.rev !diff; 

  (* We have now matched all the text in words2. 
     We need to cope with the unmatched stuff on the 
     first side, to generate the dead chunks for the second version. *)
  let chunks2_l = ref [words2] in 
  let chunks2_len  = ref 1 in 

  for c1_idx = 0 to Array.length (matched1) - 1 do 
    begin 
      let in_matched = ref true in 
      let unmatched_start = ref 0 in 
      for i1 = 0 to len1.(c1_idx) - 1 do 
	if (!in_matched) && (matched1.(c1_idx).(i1) = 0) then 
	  begin 
	    (* this is the start of an unmatched chunk *)
	    in_matched := false;
	    unmatched_start := i1; 
	  end;
	if (not !in_matched) && (matched1.(c1_idx).(i1) > 0) then 
	  begin
	    (* this is the end of an unmatched chunk *)
	    in_matched := true;
	    let l = i1 - !unmatched_start in 
	    if l >= min_dead_chunk_len then
	      (* The unmatched chunk is big enough that we make it into a dead chunk *)
	      begin
		(* Creates the word array for the dead chunk *)
		chunks2_l := (Array.sub chunks1.(c1_idx) !unmatched_start l) :: !chunks2_l;
		(* And adds the move command to the edit list *)
		diff := Mmov (!unmatched_start, c1_idx, 0, !chunks2_len, l) :: !diff;
		chunks2_len := !chunks2_len + 1
	      end
	    else 
	      (* The unmatched chunk is short, and so we just label it as deleted *)
	      diff := Mdel (!unmatched_start, c1_idx, l) :: !diff 
	  end
      done; 
      (* takes care of the last portion *)
      if (not !in_matched) then
	begin 
	  let l = len1.(c1_idx) - !unmatched_start in 
	  if l >= min_dead_chunk_len then 
	    begin
	      (* Creates the word array for the dead chunk *)
	      chunks2_l := (Array.sub chunks1.(c1_idx) !unmatched_start l) :: !chunks2_l;
	      (* And adds the move command to the edit list *)
	      diff := Mmov (!unmatched_start, c1_idx, 0, !chunks2_len, l) :: !diff;
	      chunks2_len := !chunks2_len + 1
	    end
	  else 
	    (* Last part is too short; just labels it as deleted *)
	    diff := Mdel (!unmatched_start, c1_idx, l) :: !diff 
	end
    end
  done; (* for each c1_idx *)
  
  (* Now we have to account for the new portions of words2 that are not in chunks1 *)
  (* then from words2 *)
  let in_matched = ref true in 
  let unmatched_start = ref 0 in 
  for i2 = 0 to len2 - 1 do 
    if (!in_matched) && (matched2.(i2) = 0) then 
      begin 
	(* this is the start of an unmatched chunk *)
	in_matched := false;
	unmatched_start := i2; 
      end;
    if (not !in_matched) && (matched2.(i2) <> 0) then 
      begin
	(* this is the end of an unmatched chunk *)
	in_matched := true;
	if i2 > !unmatched_start then
	  diff := Mins (!unmatched_start, 0, i2 - !unmatched_start) :: !diff
      end
  done; 
  (* takes care of the last portion *)
  if (not !in_matched) && len2 > !unmatched_start then 
    diff := Mins (!unmatched_start, 0, len2 - !unmatched_start) :: !diff; 

  (* Ok, now we have everything, and we can assemble the result *)
  
  ((Array.of_list (List.rev !chunks2_l)), !diff);;


(* **************************************************************** *)
(* Unit testing of text diff functions *)


(** Unit test for text survival *)
if false then begin
  let c_new = 1 in 
  let f_inherit live1 live2 val1 = 
    if live2 then 
      if live1 
      then val1 + 3
      else val1 + 4
    else
      if live1
      then val1 + 1
      else val1 + 2
  in
  let ts1 = "Il bene comune coincide col bene individuale." in 
  let ts2 = "Il bene comune non coincide col bene individuale." in 
  let ts3 = "Nella maggior parte dei casi, il bene comune non coincide con il bene individuale." in 
  let ts4 = "In molti casi, il bene comune non coincide completamente con quello individuale, a causa dell'egoismo delle persone." in 
  let ts5 = "In molti casi, il bene comune non coincide con quello individuale.  Questo e' causato dall'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts6 = "In generale, il bene comune non coincide con quello individuale.  La causa e' l'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts7 = "In generale, il bene comune non coincide con quello individuale, dato che le persone non badano a quello comune." in 
  let ts8 = "Volete comperare Viagra? Molto buono dato che le persone non badano a quello comune." in 
  let ts9 = ts7 in 

  let print_vals (wa: word array) (ia: int array) = 
    print_string "\n";
    if not ((Array.length wa) = (Array.length ia))
    then print_string "Arrays have different lengths!\n"
    else begin
      for i = 0 to (Array.length wa) - 1 do 
	print_string ((string_of_int ia.(i)) ^ ":" ^ wa.(i) ^ " ")
      done
    end 
  in 

  let print_chunks (wal: word array list) (ial: int array list) = 
    print_string "\nDead chunks:"; 
    List.iter2 print_vals wal ial
  in 

  let make_init_attr (wl: word array) = 
    let l = Array.length wl in 
    Array.make l 0
  in 

  let ta1 = Text.split_into_words (Vec.singleton ts1) in 
  let ta2 = Text.split_into_words (Vec.singleton ts2) in 
  let ta3 = Text.split_into_words (Vec.singleton ts3) in 
  let ta4 = Text.split_into_words (Vec.singleton ts4) in 
  let ta5 = Text.split_into_words (Vec.singleton ts5) in 
  let ta6 = Text.split_into_words (Vec.singleton ts6) in 
  let ta7 = Text.split_into_words (Vec.singleton ts7) in 
  let ta8 = Text.split_into_words (Vec.singleton ts8) in 
  let ta9 = Text.split_into_words (Vec.singleton ts9) in 

  let ia1 = make_init_attr ta1 in 

  print_vals ta1 ia1; 

  let (ia2, tc2, ic2) = text_survival ta1 ia1 [] [] ta2 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta2 ia2;
  print_chunks tc2 ic2; 

  let (ia3, tc3, ic3) = text_survival ta2 ia2 tc2 ic2 ta3 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta3 ia3;
  print_chunks tc3 ic3; 

  let (ia4, tc4, ic4) = text_survival ta3 ia3 tc3 ic3 ta4 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta4 ia4;
  print_chunks tc4 ic4; 

  let (ia5, tc5, ic5) = text_survival ta4 ia4 tc4 ic4 ta5 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta5 ia5;
  print_chunks tc5 ic5;

  let (ia6, tc6, ic6) = text_survival ta5 ia5 tc5 ic5 ta6 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta6 ia6;
  print_chunks tc6 ic6;

  let (ia7, tc7, ic7) = text_survival ta6 ia6 tc6 ic6 ta7 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta7 ia7;
  print_chunks tc7 ic7;

  print_string "\n\n";

  let c_new = 0 in 
  let f_inherit live1 live2 val1 = 
    if live2 then 
      if live1 
      then val1 + 1
      else val1 + 1
    else
      if live1
      then val1
      else val1
  in
    
  let ia1 = make_init_attr ta1 in 

  print_vals ta1 ia1; 

  let (ia2, tc2, ic2) = text_survival ta1 ia1 [] [] ta2 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta2 ia2;
  print_chunks tc2 ic2; 

  let (ia3, tc3, ic3) = text_survival ta2 ia2 tc2 ic2 ta3 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta3 ia3;
  print_chunks tc3 ic3; 

  let (ia4, tc4, ic4) = text_survival ta3 ia3 tc3 ic3 ta4 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta4 ia4;
  print_chunks tc4 ic4; 

  let (ia5, tc5, ic5) = text_survival ta4 ia4 tc4 ic4 ta5 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta5 ia5;
  print_chunks tc5 ic5;

  let (ia6, tc6, ic6) = text_survival ta5 ia5 tc5 ic5 ta6 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta6 ia6;
  print_chunks tc6 ic6;

  let (ia7, tc7, ic7) = text_survival ta6 ia6 tc6 ic6 ta7 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta7 ia7;
  print_chunks tc7 ic7;

  let (ia8, tc8, ic8) = text_survival ta7 ia7 tc7 ic7 ta8 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta8 ia8;
  print_chunks tc8 ic8;

  let (ia9, tc9, ic9) = text_survival ta8 ia8 tc8 ic8 ta9 f_inherit c_new in 
  print_string "\n\n";
  print_vals ta9 ia9;
  print_chunks tc9 ic9;

  print_string "\n\n"

end;;

(** Unit test for text tracking *)

if false then begin
  let ts1 = "Il bene comune coincide col bene individuale." in 
  let ts2 = "Il bene comune non coincide col bene individuale." in 
  let ts3 = "Nella maggior parte dei casi, il bene comune non coincide con il bene individuale." in 
  let ts4 = "In molti casi, il bene comune non coincide completamente con quello individuale, a causa dell'egoismo delle persone." in 
  let ts5 = "In molti casi, il bene comune non coincide con quello individuale.  Questo e' causato dall'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts6 = "In generale, il bene comune non coincide con quello individuale.  La causa e' l'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts7 = "In generale, il bene comune non coincide con quello individuale, dato che le persone non badano a quello comune." in 
  let ts8 = "Volete comperare Viagra? Molto buono dato che le persone non badano a quello comune." in 
  let ts9 = ts7 in 
  let ts10 = "In generale, il bene comune non coincide con quello individuale, dato che le persone non badano a quello comune se non quando gli fa comodo." in 

  let tsa = [| ts1; ts2; ts3; ts4; ts5; ts6; ts7; ts8; ts9; ts10 |] in 
  let taa = Array.map (function x -> Text.split_into_words (Vec.singleton x)) tsa in 
  let (c, l) = text_tracking [| taa.(0) |] taa.(1) in 
  Text.print_words taa.(0);
  Text.print_words taa.(1);
  print_chunks c;
  print_mdiff  l; 
  let cr = ref c in 
  for i = 2 to (Array.length taa) - 1 do begin 
    let (c, l) = text_tracking !cr taa.(i) in 
    Printf.printf "\n================\n";
    Printf.printf "New words:\n";
    Text.print_words taa.(i);
    Printf.printf "\nResulting chunks:\n";
    print_chunks c; 
    print_mdiff  l;
    cr := c
  end done

end;;


(** Unit test for edit distance *)
if false then begin 
  let e = [Ins (1, 2); Ins (4, 5); Mov (3, 4, 5); Del (3, 2)] in 
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
  let e = [Ins (1, 2); Ins (4, 5); Mov (3, 4, 5); Mov (4, 0, 2); Mov (5, 0, 5); Del (3, 2)] in 
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
  let e = [Ins (1, 2); Ins (4, 5); Mov (3, 4, 5); Mov (4, 1, 2); Mov (5, 0, 5); Del (3, 2)] in 
  print_string (string_of_float (edit_distance e 20));
  print_newline ()
end;;


(** Unit test for edit diff *)
if false then 
  begin 
    let text1a = "la capra canta contenta sotto la collina sopra la panca la capra campa" in
    let text1b = "sotto la panca la capra crepa e la capra canta" in
    
    let text2a = "nel bel mezzo del cammin di nostra vita mi trovai in una selva oscura che la diritta via era smarrita" in
    let text2b = "nel frammezzo del cammin di nostra esistenza mi trovai nel bel mezzo di una selva oscura dove la via era smarrita e non mi trovai nel cammin di casa nostra" in
    
    let text3a = "a me piace bere il caffe' dopo che mi sono svegliato" in
    let text3b = "dopo che mi sono svegliato, a me piace bere il mio caffe'" in
    
    let test_edit_diff t1 t2 = 
      let w1 = Text.split_into_words (Vec.singleton t1) in 
      let w2 = Text.split_into_words (Vec.singleton t2) in 
      let i2 = make_index_diff w2 in 
      let e = edit_diff w1 w2 i2 in 
      Text.print_words w1; 
      Text.print_words w2;  
      print_diff e
    in

    test_edit_diff text1a text1b;
    test_edit_diff text2a text2b;
    test_edit_diff text3a text3b
  end;;

(** Unit test for text distance *)
if false then begin
  let ts1 = "Il bene comune coincide col bene individuale." in 
  let ts2 = "Il bene comune non coincide col bene individuale." in 
  let ts3 = "Nella maggior parte dei casi, il bene comune non coincide con il bene individuale." in 
  let ts4 = "In molti casi, il bene comune non coincide completamente con quello individuale, a causa dell'egoismo delle persone." in 
  let ts5 = "In molti casi, il bene comune non coincide con quello individuale.  Questo e' causato dall'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts6 = "In generale, il bene comune non coincide con quello individuale.  La causa e' l'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts7 = "In generale, il bene comune non coincide con quello individuale, dato che le persone non badano a quello comune." in 
  let ts8 = "Volete comperare Viagra? Molto buono dato che le persone non badano a quello comune." in 
  let ts9 = ts7 in 
  let ts10 = "In generale, il bene comune non coincide con quello individuale. Questo e' causato dal fatto che le persone badano al loro bene privato, piuttosto che al bene comune." in

  let ta1  = Text.split_into_words (Vec.singleton ts1) in 
  let ta2  = Text.split_into_words (Vec.singleton ts2) in 
  let ta3  = Text.split_into_words (Vec.singleton ts3) in 
  let ta4  = Text.split_into_words (Vec.singleton ts4) in 
  let ta5  = Text.split_into_words (Vec.singleton ts5) in 
  let ta6  = Text.split_into_words (Vec.singleton ts6) in 
  let ta7  = Text.split_into_words (Vec.singleton ts7) in 
  let ta8  = Text.split_into_words (Vec.singleton ts8) in 
  let ta9  = Text.split_into_words (Vec.singleton ts9) in 
  let ta10 = Text.split_into_words (Vec.singleton ts10) in 

  let t = [|ta1; ta2; ta3; ta4; ta5; ta6; ta7; ta8; ta9; ta10|] in 
  let len = Array.length (t) in 
  for i = 0 to len - 1 do 
    begin
      print_newline (); 
      print_string ((string_of_int (i + 1)) ^ ": ");
      let idx = make_index_diff t.(i) in 
      for j = 0 to i - 1 do 
	begin
	  let l1 = Array.length (t.(i)) in 
	  let l2 = Array.length (t.(j)) in 
	  let l = min l1 l2 in 
	  print_string (string_of_float (edit_distance (edit_diff t.(j) t.(i) idx) l));
	  print_string " "
	end
      done
    end
  done;
  print_newline ()
end;;
    

	

(* **************************************************************** *)
(* Code for zipping together edit lists *)

(* This is to generate comparison function out of a function that 
   extracts an integer from each element to be compared *)
let comp_els toint a b = (toint a) - (toint b)


(** sort_right keeps of l only the Ins and Mov elements (which refer to w1), 
   and sorts them in order of position on the right. *)
let sort_right (l: edit list) : edit list = 
  (* Of the lhs, keep only Ins and Mov *)
  let f = function 
      Ins _ | Mov _ -> true
    | Del _ -> false
  in 
  let l' = List.filter f l in 
  let extract_pos = function 
      Ins (i, l) -> i
    | Del (i, l) -> i
    | Mov (i, j, l) -> j
  in
  List.sort (comp_els extract_pos) l'

(** symmetrically *)
let sort_left (l: edit list) : edit list = 
  (* keep only Del and Mov *)
  let f = function 
      Del _ | Mov _ -> true
    | Ins _ -> false
  in 
  let l' = List.filter f l in 
  let extract_pos = function 
      Ins (i, l) -> i
    | Del (i, l) -> i
    | Mov (i, j, l) -> i
  in
  List.sort (comp_els extract_pos) l'


(** [zip_edit_lists l r], given the edit list [l] from w1 to w2, 
    and the edit list [r] from w2 to w3, compute the best approximation 
    of the edit list from w1 to w3. *)
let zip_edit_lists (l: edit list) (r: edit list) : edit list = 
  (* The l edit list is an edit list from w0 to w1 (w0 and w1 are word lists. 
     Similarly, r is an edit list from w1 to w2. *)

  let l' = sort_right l in 
  let r' = sort_left  r in 

  (* Zips the two sorted and prepared edit lists. *)
  let rec zip l r = 
    match (l, r) with 
      (l_el :: lr, r_el :: rr) -> begin 
	(* l_el, r_el are the elements at the front of the lists *)
	match l_el with 
	  Ins (i, m) -> begin 
	    match r_el with 
	      Mov (i', j', m') -> begin (* Ins, Mov match *)
		if m = 0 || i + m - 1 < i' then zip lr r (* left all before right *)
		else if m' = 0 || i' + m' - 1 < i then zip l rr (* right all before left *)
		  (* they have an intersection; looks at which one starts first *)
		else if i < i' then zip (Ins (i', m - (i' - i)) :: lr) r 
		else if i' < i then zip l (Mov (i, j' + (i - i'), m' - (i - i')) :: rr)
		else let c = min m m' in (* equal beginnings *)
		Ins (j', c) :: zip (Ins (i + c, m - c) :: lr) (Mov (i' + c, j' + c, m' - c) :: rr)
	      end
	    | Ins (_, _) -> zip l rr (* The ins on the right is just spurious *) 
	    | Del (i', m') -> begin (* Ins, Del annihilate each other *)
		if m = 0 || i + m - 1 < i' then zip lr r (* left all before right *)
		else if m' = 0 || i' + m' - 1 < i then zip l rr (* right all before left *)
		  (* they have an intersection; looks at which one starts first *)
		else if i < i' then zip (Ins (i', m - (i' - i)) :: lr) r 
		else if i' < i then zip l (Del (i, m' - (i - i')) :: rr)
		else let c = min m m' in (* equal beginnings *)
		zip (Ins (i + c, m - c) :: lr) (Del (i' + c, m' - c) :: rr)
	      end
	  end (* Ins matching *)
	| Mov (i, j, m) -> begin 
	    match r_el with 
	      Mov (i', j', m') -> begin (* Mov, Mov match *)
		if m = 0 || j + m - 1 < i' then zip lr r (* left all before right *)
		else if m' = 0 || i' + m' - 1 < j then zip l rr (* right all before left *)
		  (* they have an intersection; looks at which one starts first *)
		else if j < i' then zip (Mov (i + (i' - j), i', m - (i' - j)) :: lr) r 
		else if i' < j then zip l (Mov (j, j' + (j - i'), m' - (j - i')) :: rr)
		else let c = min m m' in (* equal beginnings *)
		Mov (i, j', c) :: zip (Mov (i + c, j + c, m - c) :: lr) (Mov (i' + c, j' + c, m' - c) :: rr)
	      end
	    | Del (i', m') -> begin (* Mov, Del matching. Outcome is Del. *)
		if m = 0 || j + m - 1 < i' then zip lr r (* left all before right *)
		else if m' = 0 || i' + m' - 1 < j then zip l rr (* right all before left *)
		  (* they have an intersection; looks at which one starts first *)
		else if j < i' then zip (Mov (i + (i' - j), i', m - (i' - j)) :: lr) r 
		else if i' < j then zip l (Del (j, m' - (j - i')) :: rr)
		else let c = min m m' in (* equal beginnings *)
		Del (i, c) :: zip (Mov (i + c, j + c, m - c) :: lr) (Del (i' + c, m' - c) :: rr)
	      end
	    | Ins (_, _) -> zip l rr (* the Ins is spurious *)
	  end (* Mov matching *)
	| Del (_, _) -> zip lr r (* The Del is spurious *)
      end
    | (_, _) -> [] (* Not both lists full *)
  in zip l' r';;

(** Unit testing for zip *)
if false then begin 
  let l = [Del (0, 2); Mov (2, 4, 5)] in 
  let r = [Del (0, 2); Del (2, 5); Mov (7, 5, 3)] in 
  print_diff (zip_edit_lists l r);
  Printf.printf "\n"; 
  let l = [Del (0, 2); Mov (2, 4, 5); Ins (0, 4); Del (7, 3); Mov (10, 9, 3); Ins (12, 1)] in 
  let r = [Del (0, 2); Del (2, 5); Mov (7, 5, 3); Del (10, 1); Mov (11, 13, 2); 
           Ins (0, 5); Ins (8, 5); Del (13, 2)] in 
  print_diff (zip_edit_lists l r)
end;;

(** Measures how many words a difference list covers, on the left, and on the right *)
let rec diff_cover = function 
    [] -> (0, 0)
  | Ins (i, m) :: q -> let (l, r) = diff_cover q in (l, r + m)
  | Del (i, m) :: q -> let (l, r) = diff_cover q in (l + m, r)
  | Mov (i, j, m) :: q -> let (l, r) = diff_cover q in (l + m, r + m);;		      


(** Given an edit list, computes the list of blocks not affected by the edit list. 
    In [left_complement k n], [n] is the total length of the word array, and 
    [k] is how much of it we have processed already.
    The result is a Vec consisting of pairs [(offset, len)], where [offset] is the 
    offset of the block in the original string, and len is its length. *)
let rec left_complement k n = function 
    [] -> if n > k then Vec.insert 0 (k, n - k) Vec.empty else Vec.empty
  | Ins (_, _) :: rest -> left_complement k n rest 
  | Del (i, m) :: rest 
  | Mov (i, _, m) :: rest ->
      if k < i 
      then Vec.insert 0 (k, i - k) (left_complement (i + m) n rest)
      else left_complement (i + m) n rest 

(** See [left_complement] *)
let rec right_complement k n = function 
    [] -> if n > k then Vec.insert 0 (k, n - k) Vec.empty else Vec.empty
  | Del (_, _) :: rest -> right_complement k n rest 
  | Ins (i, m) :: rest
  | Mov (_, i, m) :: rest -> 
      if k < i 
      then Vec.insert 0 (k, i - k) (right_complement (i + m) n rest)
      else right_complement (i + m) n rest 

(** [make_arrays_for_local_diff c wl sep] takes a complement (left or
    right) [c], a word list [w], and a separator [sep], and produces
    two arrays [info] and [w].  A complement [c] is a list of pairs
    (i,j), where i is the offset of the pair, and j is the length.
    [info] is an array of pairs (i,j) of integers, where i is the
    block a word came from, and j is the offset from the beginning of
    the block; (-1, -1) is used to indicate that the word is a
    separator. [w] is the word array consisting of the word blocks,
    and of the separators *)
let make_arrays_for_local_diff (c: (int * int) Vec.t) (wl: word array) (sep: word) 
    : ((int * int) array) * (word array) =
  (* I deal with the case of an empty c separately *)
  if c = Vec.empty 
  then ( [| |], [| |] )
  else begin 
    (* First, it computes the length of the list *)
    let w_len = ref (-1) in (* the -1 eats the spurious +1 in the following summation *)
    for i = 0 to (Vec.length c) - 1 do begin 
      let (_, l) = Vec.get i c in 
      w_len := !w_len + l + 1
    end done; 
    (* Allocates the arrays that will be returned *)
    let info = Array.make !w_len (-1, -1) in 
    let w = Array.make !w_len sep in 
    (* Now fills the array, block by block. !offset is the position in
       the arrays to be filled next *)
    let offset = ref 0 in 
    for i = 0 to (Vec.length c) - 1 do begin 
      let (k, l) = Vec.get i c in 
      (* k is the offset of the word block; l is its length *)
      (* copies the block *)
      for j = 0 to l - 1 do begin 
	w.(!offset) <- wl.(k + j); 
	info.(!offset) <- (i, j); 
	offset := !offset + 1
      end done;
      (* This leaves space between one block and the next, with the separator in between *)
      offset := !offset + 1
    end done;
    (info, w)
  end;;

    
(** [global_from_local_diff left_iar right_iar left_compl right_compl diff_list] 
    produces a global edit list, starting from the edit list of a local comparison. 
    It takes as input: [diff_list], which is the diff list due to the local comparison, 
    [left_iar], which is the left [(int * int) array] produced by [make_arrays_for_local_diff], 
    containing the relation between local information and [left_comp], and the left 
    complement generated by [left_complement].  Same thing for the right hand side. 
    The output is a global difference list. *)
let global_from_local_diff 
    (left_iar: (int * int) array) 
    (right_iar: (int * int) array) 
    (left_compl: (int * int) Vec.t) 
    (right_compl: (int * int) Vec.t)
    (diff_list: edit list) : edit list = 
  let rec loc_to_glob = function 
      [] -> []
    | Mov (i, j, m) :: rest -> begin 
	(* Find starting point in the original lists of words *)
	let (l_block_idx, l_pos_in_block) = left_iar.(i) in 
	let (r_block_idx, r_pos_in_block) = right_iar.(j) in 
	let (l_offset, _) = Vec.get l_block_idx left_compl in 
	let (r_offset, _) = Vec.get r_block_idx right_compl in 
	let l_pos = l_offset + l_pos_in_block in 
	let r_pos = r_offset + r_pos_in_block in 
	Mov (l_pos, r_pos, m) :: (loc_to_glob rest)
      end
    | Del (i, m) :: rest -> begin 
	(* Deletes affect the lhs only, but the problem is that 
	   they can span multiple blocks. *)
	let result = ref (loc_to_glob rest) in 
	(* idx_first is the index in the Del; it is used to measure 
	   the length of Del that is matched.  It is initialized to 
	   -1 to indicate that the start of a valid match has not yet been found. 
	   Global_offset is the index in the global string *)
	let idx_first = ref 0 in 
	(* Fixes the global offset... unfortunately, I need a case analysis *)
	let global_offset = ref 0 in 
	let (l_block_idx, l_pos_in_block) = left_iar.(0) in 
	if l_block_idx > -1 then begin 
	  let (l_offset, _) = Vec.get l_block_idx left_compl in 
	  global_offset := l_offset + l_pos_in_block
	end; 
	(* Now scans the list *)
	for i = 0 to m - 1 do begin 
	  let (l_block_idx, _) = left_iar.(i) in 
	  if l_block_idx = -1 then begin 
	    (* The -1 indicates that a block has ended.  Processes it if needed (if a real block). *)
	    if i > !idx_first then result := Del (!global_offset, i - !idx_first) :: !result; 
	    idx_first := i + 1; 
	    if i + 1 < m then begin 
	      (* sets up for the next block *)
	      let (l_block_idx, l_pos_in_block) = left_iar.(i + 1) in 
	      let (l_offset, _) = Vec.get l_block_idx left_compl in 
	      global_offset := l_offset + l_pos_in_block
	    end
	  end
	end done;
	(* Ok, at this point all we have to do is add the last segment *)
	if m > !idx_first 
	then Del (!global_offset, m - !idx_first) :: !result
	else !result
      end
    | Ins (i, m) :: rest -> begin 
	(* Inserts affect the lhs only, but the problem is that 
	   they can span multiple blocks. *)
	let result = ref (loc_to_glob rest) in 
	(* idx_first is the index in the Ins; it is used to measure 
	   the length of Ins that is matched.  It is initialized to 
	   -1 to indicate that the start of a valid match has not yet been found. 
	   Global_offset is the index in the global string *)
	let idx_first = ref 0 in 
	(* Fixes the global offset... unfortunately, I need a case analysis *)
	let global_offset = ref 0 in 
	let (r_block_idx, r_pos_in_block) = right_iar.(0) in 
	if r_block_idx > -1 then begin 
	  let (r_offset, _) = Vec.get r_block_idx right_compl in 
	  global_offset := r_offset + r_pos_in_block
	end; 
	(* Now scans the list *)
	for i = 0 to m - 1 do begin 
	  let (r_block_idx, _) = right_iar.(i) in 
	  if r_block_idx = -1 then begin 
	    (* The -1 indicates that a block has ended.  Processes it if needed (if a real block). *)
	    if i > !idx_first then result := Ins (!global_offset, i - !idx_first) :: !result; 
	    idx_first := i + 1; 
	    if i + 1 < m then begin 
	      let (r_block_idx, r_pos_in_block) = right_iar.(i + 1) in 
	      let (r_offset, _) = Vec.get r_block_idx right_compl in 
	      global_offset := r_offset + r_pos_in_block
	    end
	  end
	end done;
	(* Ok, at this point all we have to do is add the last segment *)
	if m > !idx_first 
	then Ins (!global_offset, m - !idx_first) :: !result
	else !result
      end
  in loc_to_glob diff_list;; 


(** [edit_diff_using_zipped_edits wl dl wr dr]
    Given two word arrays, and two difference lists to be zipped, 
    this function returns a zipped and completed difference list. 
    [wl] is the word list on the left, [wr] on the right. 
    [dl] is the left difference list, [dr] is the right one. *)
let edit_diff_using_zipped_edits 
    (wl: word array) (wr: word array) 
    (dl: edit list)  (dr: edit list) : edit list = 
  (* First, zips the difference lists *)
  let zipped_diffs = zip_edit_lists dl dr in 
  (* Then, computes the complements, i.e., what is not covered by the joint 
     zipped list. *)
  let zl = sort_left zipped_diffs  in 
  let zr = sort_right zipped_diffs in 
  let cl = left_complement  0 (Array.length wl) zl in 
  let cr = right_complement 0 (Array.length wr) zr in 
  (* Prepares the left and right array for local differences *)
  let (info_l, local_wl) = make_arrays_for_local_diff cl wl left_separator in 
  let (info_r, local_wr) = make_arrays_for_local_diff cr wr right_separator in 
  (* Now computes the local difference between local_wl and local_wr *)
  let index_r = make_index_diff local_wr in 
  let local_diff = edit_diff local_wl local_wr index_r in 
  (* And from the local difference, computes a global one *)
  (global_from_local_diff info_l info_r cl cr local_diff) @ zipped_diffs;;


(* **************************************************************** *)
(* unit testing code for difference with zipping *)


if false then begin
  let ts0 = "Il bene individuale e' bene." in 
  let ts1 = "Il bene comune coincide col bene individuale." in 
  let ts2 = "Il bene comune non coincide col bene individuale." in 
  let ts3 = "Nella maggior parte dei casi, il bene comune non coincide con il bene individuale." in 
  let ts4 = "In molti casi, il bene comune non coincide completamente con quello individuale, a causa dell'egoismo delle persone." in 
  let ts5 = "In molti casi, il bene comune non coincide con quello individuale.  Questo e' causato dall'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts6 = "In generale, il bene comune non coincide con quello individuale.  La causa e' l'egoismo delle persone, che cercano di massimizzare il loro bene individuale, invece di quello comune." in 
  let ts7 = "In generale, il bene comune non coincide con quello individuale, dato che le persone non badano a quello comune." in 
  let ts8 = "Volete comperare Viagra? Molto buono dato che le persone non badano a quello comune." in 
  let ts9 = ts7 in 
  let ts10 = "In generale, il bene comune non coincide con quello individuale. Questo e' causato dal fatto che le persone badano al loro bene privato, piuttosto che al bene comune." in

  let ta0  = Text.split_into_words (Vec.singleton ts0) in 
  let ta1  = Text.split_into_words (Vec.singleton ts1) in 
  let ta2  = Text.split_into_words (Vec.singleton ts2) in 
  let ta3  = Text.split_into_words (Vec.singleton ts3) in 
  let ta4  = Text.split_into_words (Vec.singleton ts4) in 
  let ta5  = Text.split_into_words (Vec.singleton ts5) in 
  let ta6  = Text.split_into_words (Vec.singleton ts6) in 
  let ta7  = Text.split_into_words (Vec.singleton ts7) in 
  let ta8  = Text.split_into_words (Vec.singleton ts8) in 
  let ta9  = Text.split_into_words (Vec.singleton ts9) in 
  let ta10 = Text.split_into_words (Vec.singleton ts10) in 
 
  let w = [|ts0; ts1; ts2; ts3; ts4; ts5; ts6; ts7; ts8; ts9; ts10|] in 
  let t = [|ta0; ta1; ta2; ta3; ta4; ta5; ta6; ta7; ta8; ta9; ta10|] in 
  let len = Array.length (t) in 
  for i = 0 to len - 3 do begin
    let w0 = t.(i) in 
    for j = i + 1 to len - 2 do begin
      let w1 = t.(j) in 
      let i1 = make_index_diff w1 in 
      let e1 = edit_diff w0 w1 i1 in 
      for k = j + 1 to len - 1 do begin 
	let w2 = t.(k) in 
	let i2 = make_index_diff w2 in 
	let e2 = edit_diff w1 w2 i2 in 
	let e02 = edit_diff w0 w2 i2 in 
	(* Difference from i to k directly *)
	Printf.printf "\n================================================================\n";
	Printf.printf "Difference between %d %d %d:\n" i j k; 
	Printf.printf "String %d: %s\n" i w.(i);
	Printf.printf "String %d: %s\n" j w.(j);
	Printf.printf "String %d: %s\n" k w.(k);
	Printf.printf "------------ direct:\n"; 
	print_diff e02; 
	(* Now interpolating *)
	Printf.printf "------------ zipped:\n"; 
	print_diff (edit_diff_using_zipped_edits w0 w2 e1 e2)
      end done
    end done
  end done
end;;
