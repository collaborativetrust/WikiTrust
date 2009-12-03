/*

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
CONSEQUENTIAL DAMAGES /INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION/ HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT /INCLUDING NEGLIGENCE OR OTHERWISE/
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

 */

/* 
  Provides an ocaml interface to HDFS
*/

// caml 
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <errno.h>

#include "hdfs.h" 

/* Handle to the HDFS filesystem. */
typedef hdfsFS hdfs_fs_t;

/* HDFS file handle types. */
typedef hdfsFile hdfs_in_t;
typedef hdfsFile hdfs_out_t;

// Access functions
#define Hdfs_fs_val(v) (*((hdfs_fs_t **) Data_custom_val(v)))
#define Hdfs_in_val(v) (*((hdfs_in_t *) Data_custom_val(v)))
#define Hdfs_out_val(v) (*((hdfs_out_t *) Data_custom_val(v)))

// Memory management
void finalize_fs(value fs){
  hdfsDisconnect(Hdfs_fs_val(fs));
}

/**
void finalize_in(value file){
  sb_hdfs_in_delete(Hdfs_in_val(file));
}

void finalize_out(value file){
  sb_hdfs_out_delete(Hdfs_out_val(file));
}
*/

// Value operations
static struct custom_operations hdfs_fs_ops = {
  "edu.ucsc.trust.hdfs.fs",
  &finalize_fs,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static struct custom_operations hdfs_in_ops = {
  "edu.ucsc.trust.hdfs.in",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static struct custom_operations hdfs_out_ops = {
  "edu.ucsc.trust.hdfs.out",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

// Alloc
static value alloc_hdfs_fs(hdfs_fs_t *fs){
  value v = alloc_custom(&hdfs_fs_ops, sizeof(hdfs_fs_t *), 0, 1);
  Hdfs_fs_val(v) = fs;
  return v;
}

static value alloc_hdfs_in(hdfs_in_t file){
  value v = alloc_custom(&hdfs_in_ops, sizeof(hdfs_in_t *), 0, 1);
  Hdfs_in_val(v) = file;
  return v;
}

static value alloc_hdfs_out(hdfs_out_t file){
  value v = alloc_custom(&hdfs_out_ops, sizeof(hdfs_out_t *), 0, 1);
  Hdfs_out_val(v) = file;
  return v;
}

// Error handler -- raise Failure on error.
void handle_return_value(int res, const char *msg){
  if (res < 0){
    char *err_str;
    sprintf(err_str, "HDFS Error: %s: %s\n", strerror(errno), msg);
    caml_failwith(err_str);
  } 
}

// END STRUC BOILERPLATE

/** 
  * connect - Connect to a hdfs file system.
  * Connect to the hdfs.
  * @param host A string containing either a host name, or an ip address
  * of the namenode of a hdfs cluster. 'host' should be passed as NULL if
  * you want to connect to local filesystem. 'host' should be passed as
  * 'default' /and port as 0/ to used the 'configured' filesystem
  * /hadoop-site/hadoop-default.xml/.
  * @param port The port on which the server is listening.
  * @return Returns a handle to the filesystem or NULL on error.
  */
CAMLprim value stub_hdfs_connect(value host, value port){
  CAMLparam2(host, port);
  hdfs_fs_t *fs = hdfsConnect(String_val(host), Int_val(port));
  CAMLreturn (alloc_hdfs_fs(fs));
}

/** 
  * disconnect - Disconnect from the hdfs file system.
  * Disconnect from hdfs.
  * @param fs The configured filesystem handle.
  * @return Returns 0 on success, -1 on error.  
  */
CAMLprim value stub_hdfs_disconnect(value fs){
  CAMLparam1(fs);
  handle_return_value( hdfsDisconnect(Hdfs_fs_val(fs)), "");
  CAMLreturn (Val_unit);
}

/** 
  * open_in - Open a hdfs file for reading.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or NULL on error.
  */
CAMLprim value stub_hdfs_open_in(value fs, value path){
  CAMLparam2(fs, path);
  hdfsFile in_file = hdfsOpenFile(Hdfs_fs_val(fs), String_val(path), O_RDONLY|O_CREAT, 0, 0, 0);
  if(!in_file) {
    char *err_str;
    sprintf(err_str, "Failed to open %s for reading!\n", String_val(path));
    handle_return_value(-1, err_str);
  }
  CAMLreturn (alloc_hdfs_in(in_file));
}

/** 
  * open_out - Open a hdfs file for writing.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or raises Failure.
  */
CAMLprim value stub_hdfs_open_out(value fs, value path){
  CAMLparam2(fs, path);
  hdfsFile out_file = hdfsOpenFile(Hdfs_fs_val(fs), String_val(path), O_WRONLY|O_CREAT, 0, 0, 0);
  if(!out_file) {
    char *err_str;
    sprintf(err_str, "Failed to open %s for writting!\n", String_val(path));
    handle_return_value(-1, err_str);
  }
  CAMLreturn (alloc_hdfs_out(out_file));
}

/** 
  * close_in - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return unit.  
  */
CAMLprim value stub_hdfs_close_in(value fs, value in_file){
  CAMLparam2(fs, in_file);
  handle_return_value (hdfsCloseFile(Hdfs_fs_val(fs), Hdfs_in_val(in_file)), "");
  CAMLreturn (Val_unit);
}

/** 
  * close_out - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return Returns unit.  
  */
CAMLprim value stub_hdfs_close_out(value fs, value out_file){
  CAMLparam2(fs, out_file);
  handle_return_value (hdfsCloseFile(Hdfs_fs_val(fs), Hdfs_out_val(out_file)), "");
  CAMLreturn (Val_unit);
}

/**
  * rename - Rename file. 
  * @param fs The configured filesystem handle.
  * @param oldPath The path of the source file. 
  * @param newPath The path of the destination file. 
  * @return Returns unit. 
  */
CAMLprim value stub_hdfs_rename(value fs, value oldPath, value newPath){
  CAMLparam3(fs, oldPath, newPath);
  handle_return_value (hdfsRename(Hdfs_fs_val(fs), String_val(oldPath), String_val(newPath)), "");
  CAMLreturn (Val_unit);
}

/**
  * delete - Delete file. 
  * @param fs The configured filesystem handle.
  * @param path The path of the file. 
  * @return Returns unit. 
  */
CAMLprim value stub_hdfs_delete(value fs, value path){
  CAMLparam2(fs, path);
  handle_return_value (hdfsDelete(Hdfs_fs_val(fs), String_val(path)), "");
  CAMLreturn (Val_unit);
}

/** 
  * mkdir - Make the given file and all non-existent
  * parents into directories.
  * @param fs The configured filesystem handle.
  * @param path The path of the directory. 
  * @return Returns unit. 
  */
CAMLprim value stub_hdfs_mkdir(value fs, value path){
  CAMLparam2(fs, path);
  handle_return_value (hdfsCreateDirectory(Hdfs_fs_val(fs), String_val(path)), "");
  CAMLreturn (Val_unit);
}

/** 
  * read - Read data from an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The buffer to copy read bytes into.
  * @param length The length of the buffer.
  * @return Returns the number of bytes actually read, possibly less
  * than than length;-1 on error.
  */
CAMLprim value stub_hdfs_read(value fs, value file_in, value buf, value len){
  CAMLparam4(fs, file_in, buf, len);
  CAMLlocal1 (result);
  int i = hdfsRead(Hdfs_fs_val(fs), Hdfs_in_val(file_in), String_val(buf), Int_val(len));
  handle_return_value(i, "");
  CAMLreturn(caml_copy_int32(i));
}
//external read : hdfs_fs -> hdfs_in -> string -> int -> int = "stub_hdfs_read"

/** 
  * write - Write data into an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The data.
  * @param length The no. of bytes to write. 
  * @return Returns the number of bytes written, -1 on error.
  */
CAMLprim value stub_hdfs_write(value fs, value file_out, value buf, value len){
  CAMLparam4(fs, file_out, buf, len);
  CAMLlocal1 (result);
  int i = hdfsWrite(Hdfs_fs_val(fs), Hdfs_out_val(file_out), String_val(buf), Int_val(len));
  handle_return_value(i, "");
  CAMLreturn(caml_copy_int32(i));
}
