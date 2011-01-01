(*

Copyright (c) 2008,2010 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Bo Adler

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

let debug_level = ref 0

(** [new logger channel synch] creates an object of this class. 
    This class implements logging for the on-line implementation. 
    If [synch] is true, the logger synchs the file at every write. *)
class logger 
  (f: out_channel) 
  (synch: bool) = 
  object(self)

    (** [log s] logs the string [s]. *)
    method log (s: string) : unit = 
      output_string f s;
      if synch then flush f

    (** [flush] flushes the log to disk; this can be called once 
	every revsion is processed.  Otherwise, data loss can 
	occur, as we never close the log. *)
    method flush : unit = flush f

    (** [close] closes the log.  We would most likely never call
	this during the on-line system. *)
    method close : unit = close_out f

    (** [debug] is a conditional print based on the current debugging level *)
    method debug (level: int) (s: string) : unit =
      let now = Printf.sprintf "%f: " (Unix.gettimeofday ()) in
      if level <= !debug_level then
	self#log (now ^ s);

  end

(* Logfile *)
let online_logger = ref (new logger stdout true);;
