"""
Copyright (c) 2009 The Regents of the University of California
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
"""
This is a mod_python handler for wikitrust.

It listens for two types of requests: votes and colored_markup.

For votes, it registers the vote and returns the string "good"
For markup requests, it returns the markup if this is present.

Otherwise, it registers the need to color the revision and returns a text_not_found token.
"""

# Works with RemoteTrust for connecting to a remote wiki.

import MySQLdb
import getopt
import ConfigParser                                                         
import zlib
import gzip
import cStringIO
import time
import os

from mod_python import util
from mod_python import apache

# Fetch this via an enviromental paramiter.
BASE_DIR = ""
INI_FILE =  "/db_access_data.ini"   
DB_PREFIX = ""
not_found_text_token = "TEXT_NOT_FOUND"
sleep_time_sec = 3 

connection = None

# Compress a string buffer
def compressBuf(buf):
   zbuf = cStringIO.StringIO()
   zfile = gzip.GzipFile(mode = 'wb',  fileobj = zbuf, compresslevel = 6)
   zfile.write(buf)
   zfile.close()
   return zbuf.getvalue()

# Start a persisent connection to the db
def connect_db():
  global connection
  global DB_PREFIX
  global BASE_DIR
  global INI_FILE

  ## Parse the ini file containing the db name and password.
  ini_config = ConfigParser.ConfigParser()
  ini_config.readfp(open(BASE_DIR + INI_FILE))

  ## Initializes the DB.
  connection = MySQLdb.connect(host = ini_config.get('db', 'host'),
                               user = ini_config.get('db', 'user'), 
                               passwd = ini_config.get('db', 'pass'),
                               db = ini_config.get('db', 'db') )
 
  ## Parses the db prefix.
  DB_PREFIX = ini_config.get('db', 'prefix')

# Adds a revision into the db for coloring.
def mark_for_coloring (rev_id, page_id, user_id, rev_time, page_title):
  global DB_PREFIX
  global connection

  curs = connection.cursor()
  sql = """INSERT INTO """ + DB_PREFIX + """wikitrust_missing_revs (revision_id, page_id, page_title, rev_time, user_id) VALUES (%(rid)s, %(pid)s, %(title)s, %(time)s, %(vid)s) ON DUPLICATE KEY UPDATE requested_on = now(), processed = false"""
  args = {'rid':rev_id, 'pid':page_id, 'title':page_title, 
          'time':rev_time, 'vid':user_id }
  curs.execute(sql, args)
  connection.commit()

# Insert a vote to be processed into the db.
def handle_vote(req, rev_id, page_id, user_id, v_time, page_title):
  global DB_PREFIX
  global connection

  curs = connection.cursor()
  sql = """INSERT INTO """ + DB_PREFIX + """wikitrust_vote (revision_id, page_id, voter_id, voted_on) VALUES (%(rid)s, %(pid)s, %(vid)s, %(time)s) ON DUPLICATE KEY UPDATE voted_on = %(time)s"""
  args = {'rid':rev_id, 'pid':page_id, 'vid':user_id, 'time':v_time}
  curs.execute(sql, args)
  connection.commit()
  # Once a vote is inserted, we need to recolor the page.
  mark_for_coloring(rev_id, page_id, user_id, v_time, page_title)
  
  # Token saying things are ok
  # Votes do not return anything. This just sends back the ACK
  # that the vote was recorded.
  # We could change this to be the re-colored wiki-text, reflecting the
  # effect of the vote, if you like.
  req.write("good")


# Returns the current median value from the DB.
def get_median():
  global DB_PREFIX
  global connection

  curs = connection.cursor()
  sql = """SELECT median FROM """ + DB_PREFIX + """wikitrust_global"""
  args = ()
  numRows = curs.execute(sql, args)
  if (numRows > 0):
    dbRow = curs.fetchone()
    return dbRow[0]
  return 0.0

# Return colored text and median from the DB.
def fetch_colored_markup (rev_id, page_id, user_id, rev_time, page_title):
  global DB_PREFIX
  global not_found_text_token
  global connection

  curs = connection.cursor()
  sql = """SELECT revision_text FROM """ + DB_PREFIX + \
      """wikitrust_colored_markup  """ + \
      """ WHERE revision_id = %s"""
  args = (rev_id)
  curs.execute(sql, args)
  numRows = int(curs.rowcount)
  median = get_median()
  if (numRows > 0):
    dbRow = curs.fetchone()
    return "%f,%s" % (median,dbRow[0])
  return not_found_text_token

# Return colored text if it exists, compressed via gzip.
# If it does not exist, it returns not_found_text_token, and it adds the
# revision to the list of revisions that need coloring.
def handle_text_request (req, rev_id, page_id, user_id, rev_time, page_title):
  global DB_PREFIX
  global sleep_time_sec
  global not_found_text_token
  # First, tries to read the colored markup from the database. 
  res = fetch_colored_markup(rev_id, page_id, user_id, rev_time, page_title)
  if (res == not_found_text_token):
    # If the revision is not found among the colored ones, it marks it for coloring,
    # and it waits a bit, in the hope that it got colored.
    mark_for_coloring(rev_id, page_id, user_id, rev_time, page_title)
    time.sleep(sleep_time_sec)
  # Tries again to get it, to see if it has been colored.
  res = fetch_colored_markup(rev_id, page_id, user_id, rev_time, page_title) 
  if (res == not_found_text_token):
    # No: we will have to wait until it gets colored.  For now, we report not found.
    req.write(not_found_text_token)
  else:
    # Found: we compress it and return it.
    compressed = compressBuf(res)
    req.content_type = "application/x-gzip"
    req.content_length = len (compressed)
    req.send_http_header()
    req.write(compressed)

# Entry point for web request.
def handler(req):

  global BASE_DIR
  ## Get the base directory for the db config
  BASE_DIR = req.get_options()["WIKITRUST_BASE_DIR"]

  ## Default mimetype
  req.content_type = "text/plain" 
  # Restart the connection to the DB if its not good.
  if (connection == None):
    connect_db()

  ## Parse the form inputs.
  form = util.FieldStorage(req)
  page_id = form.getfirst("page", -1)     
  rev_id = form.getfirst("rev", -1)
  page_title =form.getfirst("page_title", "")
  time_str = form.getfirst("time", "")
  user_id = form.getfirst("user", -1)
  is_vote = form.getfirst("vote", None)

  # Sanity check on input parameters.  
  if (page_id < 0) or (rev_id < 0) or (page_title == "") or (time_str == "") \
        or (user_id < 0):
    req.write("bad")
  else:  
    if is_vote:
      handle_vote(req, rev_id, page_id, user_id, time_str, page_title)
    else:
      handle_text_request(req, rev_id, page_id, user_id, time_str, page_title)

  return apache.OK


