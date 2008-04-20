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

## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

def handler(req):

  req.content_type = "text/plain" ## needed for web based things

  form = util.FieldStorage(req)
  experiment = form.getfirst("e", None)
  file_used  = form.getfirst("file", None)
  
  ## Start out transaction going
  curs.execute("START TRANSACTION")

  ##first, if e is not set, return nothing
  if experiment is None:
    return apache.DECLINED
  
  ## otherwise, return either file info, or else mark the file as being processed
  if file_used is not None:
    curs.execute("SELECT B.file_id, B.exper_id FROM cluster_experiments \
                  AS A JOIN cluster_files_in_exper AS B ON (A.exper_id = B.exper_id) \
                  JOIN cluster_files AS C ON C.file_id = B.file_id WHERE \
                  A.exper_prefix || C.file_name || A.exper_suffix = '" + str(file_used) + "' AND \
                  A.exper_name = '" + str(experiment) + "'")

    data = curs.fetchall()
    for row in range(len(data)):
      curs.execute("UPDATE cluster_files_in_exper SET file_status = 'processing' \
                    AND processedon = now() WHERE \
                    file_id = " + row[0] + " AND exper_id = " + row[1])    
    connection.commit()
    req.write("Deleted file " + str(file_used))
    return apache.OK
  else:

    ## Start processing of a new file.
    curs.execute("SELECT A.exper_prefix || C.file_name || A.exper_suffix AS  \
                full_file_name, A.exper_ending_dir, B.file_id, B.exper_id FROM cluster_experiments \
                AS A JOIN cluster_files_in_exper AS B ON (A.exper_id = B.exper_id) \
                JOIN cluster_files AS C ON C.file_id = B.file_id WHERE B.file_status = \
                'unprocessed' AND A.exper_name = '" + str(experiment) + "' LIMIT 1")
    data = curs.fetchall()
    for row in range(len(data)):
      req.write(row[0] + FILE_ENDING_SEP + row[1])
      curs.execute("UPDATE cluster_files_in_exper SET file_status = 'processing' WHERE \
                 file_id = " + row[2] + " AND exper_id = " + row[3])

    connection.commit()  
    return apache.OK


