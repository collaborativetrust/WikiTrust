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



## creates an experiment
## Usage: ./add_expr experiment


import MySQLdb
import sys

USER = "wikiuser"
PASS = raw_input "Password? "
DB   = "wikitest"

if len(sys.argv) < 2:
  print "Usage: ./add_expr experiment"
  sys.exit(-1)

expr_name = sys.argv[1]

class Tr_db: 
  def __init__(self): 
    return
  def add_expr(self):
    try: 
      connection = MySQLdb.connect(host="localhost", 
      user=USER, passwd=PASS, db=DB ) 
      curs = connection.cursor()

      curs.execute( ''.join( [ "INSERT INTO trust_experiments (expr_name) VALUES ('"\
          , expr_name \
          , "')"] ) )

      curs.execute("SELECT expr_id FROM trust_experiments WHERE expr_name = '"+expr_name+"'");
      expr_id_int = curs.fetchone()[0]
      curs.execute("SELECT file_id FROM trust_files")
    except  MySQLdb.OperationalError, message: 
      errorMessage = "Error %d:\n%s" % (message[ 0 ], message[ 1 ] ) 
      print errorMessage
      return
    else:
      data = curs.fetchall()  
      for row in range(len(data)):
        for col in range(len(data[row])):
          file_id_int = data[row][col]
          curs.execute( "INSERT INTO trust_exper_file (expr_id, file_id) VALUES \
              ("+str(expr_id_int)+", "+str(file_id_int)+")")
      return

db = Tr_db();
db.add_expr();

