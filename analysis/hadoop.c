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

#include "hdfs.h" 

/* Handle to the HDFS filesystem. */
typedef struct sb_hdfs_fs hdfs_fs_t;

/* HDFS file handle types. */
typedef struct sb_hdfs_in hdfs_in_t;
typedef struct sb_hdfs_out hdfs_out_t;

// Access functions
#define Hdfs_fs_val(v) (*((hdfs_fs_t **) Data_custom_val(v)))
#define Hdfs_in_val(v) (*((hdfs_in_t **) Data_custom_val(v)))
#define Hdfs_out_val(v) (*((hdfs_out_t **) Data_custom_val(v)))

// Memory management
void finalize_fs(value fs){
  sb_hdfs_fs_delete(Hdfs_fs_val(fs));
}

void finalize_in(value file){
  sb_hdfs_in_delete(Hdfs_in_val(file));
}

void finalize_out(value file){
  sb_hdfs_out_delete(Hdfs_out_val(file));
}

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
  &finalize_in,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static struct custom_operations hdfs_out_ops = {
  "edu.ucsc.trust.hdfs.out",
  &finalize_out,
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

static value alloc_hdfs_in(hdfs_in_t *file){
  value v = alloc_custom(&hdfs_in_ops, sizeof(hdfs_in_t *), 0, 1);
  Hdfs_in_val(v) = file;
  return v;
}

static value alloc_hdfs_out(hdfs_out_t *file){
  value v = alloc_custom(&hdfs_out_ops, sizeof(hdfs_out_t *), 0, 1);
  Hdfs_out_val(v) = file;
  return v;
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
  CAMLlocal1 (result);
  result = caml_copy_int(hdfsDisconnect(Hdfs_fs_val(fs)));
  CAMLreturn (result);
}

/** 
  * open_in - Open a hdfs file for reading.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or NULL on error.
  */
//external open_in : hdfs_fs -> string -> hdfs_in = "stub_hdfs_open_in"

/** 
  * open_out - Open a hdfs file for writing.
  * @param fs The configured filesystem handle.
  * @param path The full path to the file.
  * @return Returns the handle to the open file or NULL on error.
  */
//external open_out : hdfs_fs -> string -> hdfs_out = "stub_hdfs_open_out"

/** 
  * close_in - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return Returns 0 on success, -1 on error.  
  */
//external close_in : hdfs_fs -> hdfs_in -> int = "stub_hdfs_close_in"

/** 
  * close_out - Close an open file. 
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @return Returns 0 on success, -1 on error.  
  */
//external close_out : hdfs_fs -> hdfs_out -> int = "stub_hdfs_close_out"

/**
  * rename - Rename file. 
  * @param fs The configured filesystem handle.
  * @param oldPath The path of the source file. 
  * @param newPath The path of the destination file. 
  * @return Returns 0 on success, -1 on error. 
  */
//external rename : hdfs_fs -> string -> string -> int = "stub_hdfs_rename"

/**
  * delete - Delete file. 
  * @param fs The configured filesystem handle.
  * @param path The path of the file. 
  * @return Returns 0 on success, -1 on error. 
  */
//external delete : hdfs_fs -> string -> int = "stub_hdfs_rename"

/** 
  * mkdir - Make the given file and all non-existent
  * parents into directories.
  * @param fs The configured filesystem handle.
  * @param path The path of the directory. 
  * @return Returns 0 on success, -1 on error. 
  */
//external mkdir : hdfs_fs -> string -> int = "stub_hdfs_mkdir"

/** 
  * read - Read data from an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The buffer to copy read bytes into.
  * @param length The length of the buffer.
  * @return Returns the number of bytes actually read, possibly less
  * than than length;-1 on error.
  */
//external read : hdfs_fs -> hdfs_in -> string -> int -> int = "stub_hdfs_read"

/** 
  * write - Write data into an open file.
  * @param fs The configured filesystem handle.
  * @param file The file handle.
  * @param buffer The data.
  * @param length The no. of bytes to write. 
  * @return Returns the number of bytes written, -1 on error.
  */
//external write : hdfs_fs -> hdfs_out -> string -> int -> int = "stub_hdfs_write" 
