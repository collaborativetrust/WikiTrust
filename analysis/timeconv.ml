(*

Copyright (c) 2007-2008 The Regents of the University of California
Copyright (c) 2010 Google Inc.
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


(** Here are functions that have to do with time *)

let is_leap_year y = ((y mod 4 = 0) && (y mod 100 <> 0)) || (y mod 400 = 0)

let yeardays_months = [| 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334; |]

(** Ocaml doesn't come with a function to convert from GMT time to Unix seconds! *)
let time_to_float (year: int) (month: int) (day: int) (hour: int) (minute: int) (second: int) : float = 
  if year < 1970 then 0.
  else begin
    let tot_days = ref 0 in
    (* Account for the year part. *)
    for y = 1970 to year - 1 do 
      if (is_leap_year y) then tot_days := !tot_days + 366 else tot_days := !tot_days + 365
    done;
    (* Account for the month part. *)
    tot_days := !tot_days + yeardays_months.(month - 1) + day - 1;
    (* And for leap years. *)
    if (is_leap_year year) && month > 2 then tot_days := !tot_days + 1;
    (float_of_int !tot_days) *. 86400. +. (float_of_int hour) *. 3600. +. 
      (float_of_int minute) *. 60. +. (float_of_int second)
  end
  
let default_time = (1970,01,01,00,00,00)

(** Converts a time as a string yyyymmddhhmmss to a time as a floating point.
    This type of time strings is found in the database. *)
let time_string_to_float (s: string) : float =
  let (yy,mm,dd,h,m,s) = begin
    try
      let yy' = int_of_string (String.sub s  0 4) in 
      let mm' = int_of_string (String.sub s  4 2) in 
      let dd' = int_of_string (String.sub s  6 2) in 
      let h'  = int_of_string (String.sub s  8 2) in 
      let m'  = int_of_string (String.sub s 10 2) in 
      let s'  = int_of_string (String.sub s 12 2) in 
      (yy',mm',dd',h',m',s')
    with Invalid_argument _ -> default_time
  end in time_to_float yy mm dd h m s

(** Converts a time as a string yyyymmddhhmmss to a time as a timestamp.
    This type of time strings is found in the database. *)
let time_string_to_timestamp (s: string) : (int * int * int * int * int * int) = 
  let yy = int_of_string (String.sub s  0 4) in 
  let mm = int_of_string (String.sub s  4 2) in 
  let dd = int_of_string (String.sub s  6 2) in 
  let h  = int_of_string (String.sub s  8 2) in 
  let m  = int_of_string (String.sub s 10 2) in 
  let s  = int_of_string (String.sub s 12 2) in 
  (yy, mm, dd, h, m, s)

(** Compare two times as float, i.e. for sorting purposes *)
let cmp (t1: float) (t2: float) : int = compare t1 t2

(** Gets the time of a float *)
let float_to_time f = 
  let tm = Unix.gmtime f in 
  ((1900 + tm.Unix.tm_year), (1 + tm.Unix.tm_mon), tm.Unix.tm_mday, tm.Unix.tm_hour, tm.Unix.tm_min, tm.Unix.tm_sec);;

(* Conversion from time string in xml dump to time string in database *)
let compact_time_string (s: string) : string =
  let yy = String.sub s  0 4 in 
  let mm = String.sub s  5 2 in 
  let dd = String.sub s  8 2 in 
  let h  = String.sub s 11 2 in 
  let m  = String.sub s 14 2 in 
  let s  = String.sub s 17 2 in 
  yy ^ mm ^ dd ^ h ^ m ^ s

(* Conversion of time in xml dump *)
let convert_time str =
  let year   = (try (int_of_string 
			(try (String.sub str  0 4) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  let month  = (try (int_of_string
			(try (String.sub str  5 2) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  let day    = (try (int_of_string 
			(try (String.sub str  8 2) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  let hour   = (try (int_of_string 
			(try (String.sub str 11 2) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  let minute = (try (int_of_string 
			(try (String.sub str 14 2) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  let second = (try (int_of_string 
			(try (String.sub str 17 2) with (Invalid_argument "String.sub") -> "0")) 
    with (Failure "int_of_string") -> 0) in
  time_to_float year month day hour minute second;;

(* Testing *)
if false then begin 
  let (y, m, d, hh, mm, ss) = float_to_time (time_to_float 2007 06 05 09 43 12) in 
  Printf.printf "%d/%d/%d %d:%d:%d\n" d m y hh mm ss;
  (* Captain America example *)
  let s1 = "20070311025946" in
  let s2 = "20070311035738" in
  Printf.printf "Converting s1: %S\n" s1;
  let f1 = time_string_to_float s1 in
  Printf.printf "f1: %f\n" f1;
  let (yy, mm, dd, h, m, s) = float_to_time f1 in 
  Printf.printf "And back to s1: %d/%d/%d %d:%d:%d\n" yy mm dd h m s;
  Printf.printf "Converting s2: %S\n" s2;
  let f2 = time_string_to_float s2 in
  Printf.printf "f2: %f\n" f2;
  let (yy, mm, dd, h, m, s) = float_to_time f2 in 
  Printf.printf "And back to s2: %d/%d/%d %d:%d:%d\n" yy mm dd h m s;
end;;

