(*

Copyright (c) 2009 Luca de Alfaro.
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

(* This file contains functions to read a robots file, returning a
   hashtable that permits fast checking of whether a user is a robot.
   The hashtable maps usernames to unit; if a username is found in the
   hashtable, the user is a robot.
   The format of the robot file is one line per robot, with the robot
   name encoded as an Ocaml string, as in ("" included:)
   "a funny\trobot name"
 *)

type robot_set_t = (string, unit) Hashtbl.t

let empty_robot_set : robot_set_t = Hashtbl.create 100

let read_robot_file (file_name: string) : robot_set_t =
  let robots : robot_set_t = empty_robot_set in
  let in_file = open_in file_name in
  begin
    try (* Until we get an End_of_file. *)
      while true do begin
	let line = input_line in_file in
	(* Extracts the robot name *)
	let get_robot s = Hashtbl.add robots s () in
	try 
	  Scanf.sscanf line "%S" get_robot
	with Scanf.Scan_failure _ -> begin
	  output_string stderr ("Error reading line: " ^ line)
	end
      end done
    with End_of_file -> close_in in_file
  end;
  robots


	 
