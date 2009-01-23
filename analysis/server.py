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
import ConfigParser                                                             import zlib          

from mod_python import util
from mod_python import apache

BASE_DIR = "."                                                     
INI_FILE = BASE_DIR + "db_options.ini"   
FILE_ENDING_SEP = " "
DB_PREFIX = ""
not_found_text_token = "TEXT_NOT_FOUND"
sleep_time_sec = 3 

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

def mark_for_coloring(rev_id, page_id, user_id, rev_time, page_title):
  sql = """INSERT INTO %(prefix)swikitrust_missing_revs (revision_id, page_id, page_title, rev_time, user_id) VALUES (%(rid)d, %(pid)d, %(title)s, %(time)s, %(vid)d) ON DUPLICATE KEY UPDATE requested_on = now(), processed = false"""
  args = {'prefix':DB_PREFIX, 'rid':rev_id, 'pid':page_id, 'title':page_title, 
          'time':v_time, 'vid':user_id }
  curs.execute(sql, args)

def handle_vote(req, rev_id, page_id, user_id, v_time, page_title):
  sql = """INSERT INTO %(prefix)swikitrust_vote (revision_id, page_id, voter_id, voted_on) VALUES (%(rid)d, %(pid)d, %(vid)d, %(time)s)"""
  args = {'prefix':DB_PREFIX, 'rid':rev_id, 'pid':page_id, 'vid':user_id,
          'time':v_time}
  curs.execute(sql, args)

  mark_for_coloring(rev_id, page_id, user_id, rev_time, page_title)
  
  req.write("good")

def fetch_colored_markup(rev_id, page_id, user_id, rev_time, page_title):
  sql = """SELECT revision_text FROM %swikitrust_colored_markup WHERE revision_id = %s"""
  args = {}
  numRows = curs.execute(sql, args)

  if (numRows > 0):
    dbRow = curs.fetchone()  
    return dbRow[0]
  return not_found_text_token

def handle_text_request(req, rev_id, page_id, user_id, rev_time, page_title):
  res = fetch_colored_markup(rev_id, page_id, user_id, rev_time, page_title)
  if (res == not_found_text_token):
    mark_for_coloring(rev_id, page_id, user_id, rev_time, page_title)
    time.sleep(sleep_time_sec)
   
  res = fetch_colored_markup(rev_id, page_id, user_id, rev_time, page_title) 
  if (res == not_found_text_token):
    req.write(not_found_text_token)
  else:
    compressed = compress(res)
    req.content_type = "application/x-gzip"
    req.content_length = len (compressed)
    req.write(compressed)
  
connect_db()

def handler(req):

  req.content_type = "text/plain" ## needed for web based things

  ## Parse the form inputs
  form = util.FieldStorage(req)
  page_id = form.getfirst("page", -1)     
  rev_id = form.getfirst("rev", -1)
  page_title =form.getfirst("page_title", "")
  time_str = form.getfirst("time", "")
  user_id = form.getfirst("user", -1)
  is_vote = form.getfirst("vote", None)
  
  if is_vote:
    handle_vote(req, rev_id, page_id, user_id, time_str, page_title)
  else:
    handle_text_request(req, rev_id, page_id, user_id, time_str, page_title)

  return apache.OK


