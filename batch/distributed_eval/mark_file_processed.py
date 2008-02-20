"""
Copyright (c) 2007-2008 The Regents of the University of California
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

"""

# Given a file and experiment, deletes the file from the experiment

import MySQLdb

## start up a web connection
## note -- this is really insecure -- there must be a way to do better
## Ian Pye <ipye@cs.ucsc.edu>

USER = "root"
PASS = raw_input "Password? "
DB   = "wikidb"
SEP  = ","

connection = MySQLdb.connect(host="localhost",
user=USER, passwd=PASS, db=DB )
curs = connection.cursor()

def index(req, e=None, file=None):
  req.content_type = "text/plain" ## needed for web based things

  ret_val = 0;

  ##first, if e is not set, return nothing
  if e is None:
    return -1
  if file is None:
    return -2
  
  ## otherwise, pull out some file ids to return
  curs.execute("select B.file_id, B.expr_id from trust_files as A \
                  join trust_exper_file as B on A.file_id = B.file_id join  \
                  trust_experiments as C on B.expr_id = C.expr_id where expr_name = '"+e+"' AND filename = '"+file+"'");

  data = curs.fetchall()
  for row in range(len(data)):
    curs.execute("DELETE FROM trust_exper_file WHERE file_id = "+str(data[row][0]) \
                    + " AND expr_id = "+str(data[row][1]) );
    ret_val = 1

  return ret_val


