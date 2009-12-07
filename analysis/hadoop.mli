(*

Copyright (c) 2009 Ian Pye
All rights reserved.

Authors: Ian Pye

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

(* 
  Provides an interface to HDFS
*)

(* Handle to the HDFS filesystem. *)
type hdfs_fs

(* HDFS file handle types. *)
type hdfs_out
type hdfs_in

(** 
  * connect - Connect to a hdfs file system.
  * Connect to the hdfs.
  * @param host A string containing either a host name, or an ip address
  * of the namenode of a hdfs cluster. 'host' should be passed as NULL if
  * you want to connect to local filesystem. 'host' should be passed as
  * 'default' (and port as 0) to used the 'configured' filesystem
  * (hadoop-site/hadoop-default.xml).
  * @param port The port on which the server is listening.
  * @return Returns a handle to the filesystem or NULL on error.
  *)
external connect : string -> int -> hdfs_fs = "stub_hdfs_connect"

(** 
  * disconnect - Disconnect from the hdfs file system.
  * Disconnect from hdfs.
  * @param fs The configured filesystem handle.
  * @return Returns 0 on success, -1 on error.  
  *)
external disconnect : hdfs_fs -> unit = "stub_hdfs_disconnect"

(** 
  * open_in - Open a hdfs file for reading.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or NULL on error.
  *)
external open_in : hdfs_fs -> string -> hdfs_in = "stub_hdfs_open_in"

(** 
  * open_out - Open a hdfs file for writing.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or NULL on error.
  *)
external open_out : hdfs_fs -> string -> hdfs_out = "stub_hdfs_open_out"

(** 
  * close_in - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return Returns 0 on success, -1 on error.  
  *)
external close_in : hdfs_fs -> hdfs_in -> unit = "stub_hdfs_close_in"

(** 
  * close_out - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return Returns 0 on success, -1 on error.  
  *)
external close_out : hdfs_fs -> hdfs_out -> unit = "stub_hdfs_close_out"

(**
  * rename - Rename file. 
  * @param fs The configured filesystem handle.
  * @param oldPath The path of the source file. 
  * @param newPath The path of the destination file. 
  * @return Returns 0 on success, -1 on error. 
  *)
external rename : hdfs_fs -> string -> string -> unit = "stub_hdfs_rename"

(**
  * delete - Delete file. 
  * @param fs The configured filesystem handle.
  * @param path The path of the file. 
  * @return Returns 0 on success, -1 on error. 
  *)
external delete : hdfs_fs -> string -> unit = "stub_hdfs_rename"

(** 
  * mkdir - Make the given file and all non-existent
  * parents into directories.
  * @param fs The configured filesystem handle.
  * @param path The path of the directory. 
  * @return Returns 0 on success, -1 on error. 
  *)
external mkdir : hdfs_fs -> string -> unit = "stub_hdfs_mkdir"

(** 
  * read - Read data from an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The buffer to copy read bytes into.
  * @param length The length of the buffer.
  * @return Returns the number of bytes actually read, possibly less
  * than than length;-1 on error.
  *)
external input : hdfs_fs -> hdfs_in -> string -> int -> int = "stub_hdfs_read"

(** 
  * write - Write data into an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The data.
  * @param length The no. of bytes to write. 
  * @return Returns the number of bytes written, -1 on error.
  *)
external output : hdfs_fs -> hdfs_out -> string -> int -> int = "stub_hdfs_write" 

(** Does the path exist? *)
external exists : hdfs_fs -> string -> bool = "stub_hdfs_exists"
