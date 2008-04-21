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
import getopt
import ConfigParser                                                                       
from mod_python import util
from mod_python import apache

BASE_DIR = "/var/www/cluster_run_handler/"                                                     
INI_FILE = BASE_DIR + "db_options.ini"   
FILE_ENDING_SEP = " "

connection = None
curs = None

def connect_db():
  global connection
  global curs

  ## parse the ini file
  ini_config = ConfigParser.ConfigParser()
  ini_config.readfp(open(INI_FILE))

  ## init the DB
  connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
  user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
      , db=ini_config.get('db', 'db') )
  curs = connection.cursor()

connect_db()

def handler(req):

  req.content_type = "text/plain" ## needed for web based things

  form = util.FieldStorage(req)
  file_used  = form.getfirst("file", None)
  
  ## Start out transaction going
  try:
    connect_db()
    curs.execute("START TRANSACTION")
  except Exception: ## Yes, in general I know this is bad form.
    connect_db()
    curs.execute("START TRANSACTION")  

  ## otherwise, return either file info, or else mark the file as being processed
  if file_used is not None:

    curs.execute("UPDATE cluster_simple SET processedon = now(), file_status = \
                  'processed' WHERE file_name = %s", (str(file_used)))
    connection.commit()
    req.write("Deleted file " + str(file_used))
    return apache.OK
  else:

    ## Start processing of a new file.
    curs.execute("SELECT file_name, file_return_dir FROM cluster_simple WHERE \
                  file_status = 'unprocessed' LIMIT 1")
    data = curs.fetchall()
    for row in range(len(data)):
      req.write(data[row][0] + FILE_ENDING_SEP + data[row][1])
      curs.execute("UPDATE cluster_simple SET file_status = 'processing' WHERE \
                    file_name = '" + data[row][0] + "'")

    connection.commit()  
    return apache.OK


