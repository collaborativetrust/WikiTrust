#!/usr/bin/python

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


## adds files into the db to be processed
## Ian Pye <ipye@cs.ucsc.edu>

import MySQLdb, sys

USER = "wikiuser"
PASS = raw_input "Password? "
DB   = "wikitest"

START = 1
END   = 12
DO_PADDING = False

if len(sys.argv) < 3:
  print "Usage: ./add_files prefix suffix <start> <end> <paadding>"
  sys.exit(-1)

file_prefix = sys.argv[1]
file_suffix = sys.argv[2]

if len( sys.argv ) > 3 :
  START = int(sys.argv[3])
if len( sys.argv ) > 4:
  END =  int(sys.argv[4])
if len( sys.argv ) > 5 :
  DO_PADDING = True

class Tr_db: 
  def __init__(self): 
    return
  def add_entries(self):
    try: 
      connection = MySQLdb.connect(host="localhost", 
      user=USER, passwd=PASS, db=DB ) 
      curs = connection.cursor()
      for i in range(START, END):
        if DO_PADDING :
          curs.execute( ''.join( [ "INSERT INTO trust_files (filename) VALUES ('"
              , file_prefix
              , '%(#)05d' % {'#':i } 
              , file_suffix
              , "')"] ) )
        else:   
          curs.execute( ''.join( [ "INSERT INTO trust_files (filename) VALUES ('"
              , file_prefix
              , str(i)
              , file_suffix
              , "')"] ) )   
    except  MySQLdb.OperationalError, message: 
      errorMessage = "Error %d:\n%s" % (message[ 0 ], message[ 1 ] ) 
      print errorMessage
      return
    else:
      return

db = Tr_db();
db.add_entries();

