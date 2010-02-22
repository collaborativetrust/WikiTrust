(***********************************************************************)
(*                                                                     *)
(* Memcached Client library module                                     *)
(*                                                                     *)
(***********************************************************************)

(*
 *  supported plan:
 *    "set", "add", "replace"
 *    "get" and "gets"
 *    "delete"
 *    "incr" and "decr"
 *)

TYPE_CONV_PATH "UCSC_WIKI_RESEARCH"    

type t = {
  mutable hostname : string;
  mutable port     : int ;
  mutable sock     : Unix.file_descr;
}

let version = "0.0.1"
    
let ends_with s suff = 
  if String.length(s) < String.length(suff) then
    false
  else
    let suff_len = String.length suff in
    let s_suffix = String.sub s ((String.length s) - suff_len) suff_len in
      if String.compare s_suffix suff == 0 then true
      else  false
        
let tcp_recv sock =
  let response = Buffer.create 256 in
  let buff = String.create 128 in
  let rec read_loop () =
    let r = Unix.read sock buff 0 127 in
      if r < 1 then ()
      else (
        Buffer.add_string response (String.sub buff 0 r);
        if ends_with (Buffer.contents response) "\r\n" then (
          ();
        ) else (
          read_loop ();
          )
      )
  in
    read_loop ();
    Buffer.contents response
      
let open_connection hostname port =
  Printf.printf "<open_connection> %s:%d\n" hostname port;
  let conn = {
    hostname = hostname;
    port = port;
    sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0;
  } in 
  let haddr = Unix.gethostbyname conn.hostname in
    Unix.connect conn.sock (Unix.ADDR_INET(haddr.Unix.h_addr_list.(0), conn.port));
    conn
      
let get self key =
    Printf.printf "<get> key=[%s]\n" key;
  let request = Printf.sprintf "get %s\r\n" key in
  let r = Unix.write self.sock request 0 (String.length request) in
    if r < 0 then ""
    else
      let res = tcp_recv self.sock in
      let reg = Str.regexp "VALUE \\([^ ]+?\\) \\([^ ]+?\\) \\([^ ]+?\\)\\([^\r\n]*\\)\r\n\\(.+?\\)\r\nEND\r\n"  in
      let r = Str.string_match reg res 0 in
        if r == false then ""
        else Str.matched_group 5 res
          
let set self key value =
  Printf.printf "<set> key=[%s], value=[%s]\n" key value;
  let request = Printf.sprintf "set %s 0 0 %d\r\n%s\r\n" key (String.length value) value in
  let r = Unix.write self.sock request 0 (String.length request) in
    if r < 0 then ()
    else
      let res = tcp_recv self.sock in
        Printf.printf "<get> response: [%s]\n" res;
        ()
          
let add self key value =
  Printf.printf "<add> key=[%s], value=[%s]\n" key value;
  let request = Printf.sprintf "add %s 0 0 %d\r\n%s\r\n" key (String.length value) value in
  let r = Unix.write self.sock request 0 (String.length request) in
    if r < 0 then ()
    else
      let res = tcp_recv self.sock in
        Printf.printf "<add> response: [%s]\n" res;
        ()
          
          
let delete self key =
  Printf.printf "<delete> key=[%s]\n" key;
    let request = Printf.sprintf "delete %s\r\n" key in
    let r = Unix.write self.sock request 0 (String.length request) in
      if r < 0 then ()
      else
        let res = tcp_recv self.sock in
        let reg = Str.regexp "^DELETED"  in
        let r = Str.string_match reg res 0 in
          if r == false then ()
          else ()
            
let close_connection self =
  Printf.printf "<close_connection>\n";
  Unix.shutdown self.sock Unix.SHUTDOWN_ALL;
  Unix.close self.sock
    
let make_revision_text_key (rev_id : int) (db_name : string) : string =
  (* TODO(Bo): Doesn't this key need to match WikiTrust.pm line 460? *)
  Printf.sprintf "%s:revisiontext:revid:%d" db_name rev_id
