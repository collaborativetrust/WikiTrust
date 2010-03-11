(*

Copyright (c) 2008 The Regents of the University of California
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

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"

(* editlist.ml : this file contains the types related to edit lists *)

type edit = 
    Ins of int * int  (* Ins (i, l) means add l words at position i *)
  | Del of int * int  (* Del (i, l) means delete l words from position i *)
      (* Mov (i, j, l) means move l words from pos i to pos l *)
  | Mov of int * int * int 
with sexp 

(* same as edit, but for the case when the lhs and rhs are lists of chunks *)
type medit = 
    (* Mins (i, l) means insert l words at pos i of chunk 0 *)
    Mins of int * int 
      (* Mdel (i, k, l) means del l words at pos i of chunk k *)
  | Mdel of int * int * int 
      (* Mmov (i, k, j, n, l) means mov l words from pos i of chunk k
	 to pos j of chunk n *)
  | Mmov of int * int * int * int * int 


(** Useful for debugging purposes *)
let rec diff_to_string l : string = 
  match l with 
    d :: l' ->
      begin
	let s = match d with 
	    Ins (i, l) -> Printf.sprintf "Ins(%d, %d) " i l 
	  | Del (i, l) -> Printf.sprintf "Del(%d, %d) " i l 
	  | Mov (i, j, l) -> Printf.sprintf "Mov(%d, %d, %d) " i j l 
	in s ^ diff_to_string l'
      end
  | [] -> "";;

let rec mdiff_to_string l : string = 
  match l with 
    d :: l' ->
      begin 
	let s = match d with 
	    Mins (i, l) -> Printf.sprintf "Ins(%d, 0) %d " i l 
	  | Mdel (i, k, l) -> Printf.sprintf "Del(%d, %d) %d " i k l 
	  | Mmov (i, k, j, n, l) -> Printf.sprintf "Mov(%d, %d) (%d, %d) %d " i k j n l 
	in s ^ mdiff_to_string l'
      end
  | [] -> "";;

let print_diff l = Printf.printf "\n%s" (diff_to_string l);;
let print_mdiff l = Printf.printf "\n%s" (mdiff_to_string l);;


(* **************************************************************** *)
(* Edit distance computation *)

(* A graph of edit lists consists in two related arrays:
   an array of edit list elements, and an array of edges from that element.
   The edges are stored by storing their destination. *)
type graph = edit array * (int list) array

(* An index for the graph is a hashtable that, given a false (left) or
   true (right), and an index in the words, tells which element begins
   or ends at that position. *)
type index = (bool * int , int) Hashtbl.t

(* Creates an index for the graph *)
let create_index (g: graph) : index =
  let (g_elements, _) = g in
  let h : index = Hashtbl.create 10 in
  (* Function f is iterated on the elements of the graph. *)
  let f (i: int) = function
      Del (n, k) -> begin 
	Hashtbl.add h (false, n) i;
	Hashtbl.add h (false, n + k - 1) i
      end
    | Ins (n, k) -> begin
	Hashtbl.add h (true, n) i;
	Hashtbl.add h (true, n + k - 1) i
      end
    | Mov (n, m, k) -> begin
	Hashtbl.add h (false, n) i;
	Hashtbl.add h (false, n + k - 1) i;
	Hashtbl.add h (true, m) i;
	Hashtbl.add h (true, m + k - 1) i
      end
  in Array.iteri f g_elements;
  h

(* Creates edges for the graph.  Takes an input a graph, its index,
   and the length of the string on the lhs and rhs. *)
let compute_edges (g: graph) (h: index) (len_lhs: int) (len_rhs: int) : unit =
  let (g_elements, g_edges) = g in
  let add_edge i j = begin
    g_edges.(i) <- j :: g_edges.(i);
    g_edges.(j) <- i :: g_edges.(j)
  end in 
  (* The function f is iterated on the elements of the graph,
     and connects them to other elements, using the index
     to find them. *)
  let f (i: int) = function
      Del (n, k) -> begin
	(* First, we do the upper extremity of a Del. *)
	if n = 0 then begin
	  (* A Del at the beginning is connected to the beginning on
	     the other side. *)
	  if Hashtbl.mem h (true, 0) then begin
	    let j = Hashtbl.find h (true, 0) in
	    match g_elements.(j) with 
	      Ins (0, _) -> add_edge i j;
	    | _ -> ()
	  end
	end else begin
	  (* Non-zero beginning: look at what is above. *)
	  if Hashtbl.mem h (false, n - 1) then begin
	    let j = Hashtbl.find h (false, n - 1) in
	    match g_elements.(j) with
	      Del _ -> add_edge i j;
	    | Mov (n', m', k') -> 
		(* Finds what ends at the bottom of the Mov on the right. *)
		if m' + k' < len_rhs then begin
		  if Hashtbl.mem h (true, m' + k') then begin
		    let z = Hashtbl.find h (true, m' + k') in
		    match g_elements.(z) with
		      Ins _ -> add_edge i z;
		    | _ -> ()
		  end
		end
	    | Ins _ -> ()
	  end
	end;
	(* Then, we do the lower extremity.  Notice we don't need to
	   worry about Dels, as we do them in the above case. *)
	if n + k < len_lhs then begin
	  (* There is something below. *)
	  if Hashtbl.mem h (false, n + k) then begin
	    let j = Hashtbl.find h (false, n + k) in
	    match g_elements.(j) with
	      Mov (n', m', k') ->
		(* Finds what is at the top of the Mov on the right. *)
		if m' > 0 then begin
		  if Hashtbl.mem h (true, m' - 1) then begin
		    let z = Hashtbl.find h (true, m' - 1) in
		    match g_elements.(z) with
		      Ins _ -> add_edge i z
		    | _ -> ()
		  end
		end
	    | _ -> ()
	  end
	end else begin
	  (* There is nothing below; this is connected with what is at the
	     end on the other side. *)
	  if Hashtbl.mem h (true, len_rhs - 1) then begin
	    let j = Hashtbl.find h (true, len_rhs - 1) in
	    match g_elements.(j) with
	      Ins _ -> add_edge i j
	    | _ -> ()
	  end
	end
      end
    | Ins (n, _) -> 
	if n > 0 then begin
	  if Hashtbl.mem h (true, n - 1) then begin
	    let j = Hashtbl.find h (true, n - 1) in
	    match g_elements.(j) with
	      Ins _ -> add_edge i j
	    | _ -> ()
	  end
	end
    | Mov _ -> ()
  in Array.iteri f g_elements


(* Measures the contributions of each group of connected Ins/Del to the total distance. *)
let measure_insdel (g: graph) (h: index) : float =
  let total_contribution = ref 0. in
  let (g_elements, g_edges) = g in
  let n = Array.length g_elements in
  let processed = Array.create n false in
  (* Scans the graph, finding the connected component starting at each i *)
  for i = 0 to n - 1 do begin
    if not processed.(i) then begin
      (* Computes the component that includes i *)
      let component = ref [i] in
      processed.(i) <- true;
      let open_list = ref g_edges.(i) in
      while !open_list != [] do begin
	let el = List.hd !open_list in
	open_list := List.tl !open_list;
	if not processed.(el) then begin
	  component := el :: !component;
	  processed.(el) <- true;
	  open_list := List.rev_append !open_list g_edges.(el)
	end
      end done;
      (* Ok, now we have a connected component.  Measure the total Ins and Del in it. *)
      let total_ins = ref 0 in
      let total_del = ref 0 in
      let f i = match g_elements.(i) with
	  Ins (n, k) -> total_ins := !total_ins + k
	| Del (n, k) -> total_del := !total_del + k
	| _ -> ()
      in List.iter f !component;
      (* debug *)
      (* Printf.printf "In component: ins = %d , del = %d \n" !total_ins !total_del; *)
      (* Finally, measures the contribution. *)
      let max_c = float_of_int (max !total_ins !total_del) in
      let min_c = float_of_int (min !total_ins !total_del) in
      total_contribution := !total_contribution +. max_c -. 0.4 *. min_c
    end
  end done;
  !total_contribution


let contribution_insdel edits : float =
  (* Creates the graph. *)
  let g_elements = Array.of_list edits in
  let n = Array.length g_elements in
  let g_edges = Array.make n [] in
  let g = (g_elements, g_edges) in
  (* And the index *)
  let h = create_index g in
  (* Builds the edges. *)
  let len_lhs = ref 0 in
  let len_rhs = ref 0 in
  let f = function
      Ins (n, k) -> len_rhs := max !len_rhs (n + k)
    | Del (m, k) -> len_lhs := max !len_lhs (m + k)
    | Mov (n, m, k) -> begin
	len_rhs := max !len_rhs (n + k);
	len_lhs := max !len_lhs (m + k)
      end
  in List.iter f edits;
  compute_edges g h !len_lhs !len_rhs;
  (* Measures the contribution. *)
  measure_insdel g h

  
let contribution_mov edits l : float =
  let rec filter_mov (el: edit list) : (int * int * int) list =
    match el with 
      [] -> []
    | e :: l -> begin
	match e with 
	  Mov (i, j, m) -> (i, j, m) :: (filter_mov l)
	| Ins (i, len) 
	| Del (i, len) -> filter_mov l
      end
  in
  let mov_l = filter_mov edits in 
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
  let mov' = float_of_int !tot_mov in 
  let len' = (if l = 0 then 1.0 else float_of_int l) in 
  (mov' /. len');;

let edit_distance (edits: edit list) (l: int) : float = 
  let id_contr = contribution_insdel edits in
  let mov_contr = contribution_mov edits l in
  id_contr +. mov_contr


(** Unit test for edit distance *)
if false then begin 
  let e = [Mov (0, 0, 2); Mov (6, 4, 3); Del (2, 4); Ins (2, 2)] in
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
  let e = [Del (0, 2); Mov (2, 0, 3); Ins (3, 3)] in
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
  let e = [Del (0, 2); Del (4, 2); Del (6, 1); Del (11, 3); Mov (2, 0, 2); Mov (7, 2, 2); 
           Mov (9, 9, 2); Ins(4, 2); Ins (6, 3); Ins (11, 3)] in
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
  let e = [Mov (2, 7, 1); Mov (4, 2, 2); Mov (9, 5, 1); Del (0, 2); Del (3, 1); Del (6, 3);
           Ins(0, 2); Ins (4, 1); Ins (6, 1); Ins (8, 2)] in
  print_string (string_of_float (edit_distance e 20));
  print_newline ();
end;;


