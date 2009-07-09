#!/usr/bin/python

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

Clears out the database, and then adds vote, edit and coloring not found requests
to the DB.

Designed as a run-harness to test the dispatcher.

"""

import MySQLdb
import sys
import getopt
import os
import ConfigParser

BASE_DIR = "./"
INI_FILE = BASE_DIR + "db_access_data.ini"
DB_PREFIX = "" 

connection = None

## Events to add
VOTE = 0
TEXT = 5
EDIT = 10

PAGE_ID = 1
PAGE_TITLE = "testing title"

events = [{'type':TEXT,'rev_id':1,'page_id':PAGE_ID,'user_id':1,'v_time':'10',
           'page_title':PAGE_TITLE},
          {'type':TEXT,'rev_id':2,'page_id':PAGE_ID,'user_id':2,'v_time':'15',
           'page_title':PAGE_TITLE},
          {'type':EDIT,'rev_id':3,'page_id':PAGE_ID,'user_id':3,'v_time':'20',
           'page_title':PAGE_TITLE},
          {'type':VOTE,'rev_id':4,'page_id':PAGE_ID,'user_id':3,'v_time':'25',
           'page_title':PAGE_TITLE},
          {'type':VOTE,'rev_id':5,'page_id':PAGE_ID,'user_id':4,'v_time':'30',
           'page_title':PAGE_TITLE},
          {'type':TEXT,'rev_id':6,'page_id':PAGE_ID,'user_id':5,'v_time':'35',
           'page_title':PAGE_TITLE}]

## Use the methods defined in the server.py file
sys.path.append(BASE_DIR + "../remote/analysis")
import server_utils

## Usage method
def usage():
  print "Usage: python wmf_test_harness.py"  

## Add a vote
def addVote(rev_id, page_id, user_id, v_time, page_title):
  global DB_PREFIX
  global connection

  curs = connection.cursor()
  sql = """INSERT INTO """ + DB_PREFIX + """wikitrust_vote (revision_id, page_id, voter_id, voted_on) VALUES (%(rid)s, %(pid)s, %(vid)s, %(time)s) ON DUPLICATE KEY UPDATE voted_on = %(time)s"""
  args = {'rid':rev_id, 'pid':page_id, 'vid':user_id, 'time':v_time}
  numRows = curs.execute(sql, args)
  connection.commit()
  # Once a vote is inserted, we need to recolor the page.
  # Only do this though if the vote is not a repeat.
  if (numRows > 0):
     server_utils.mark_for_coloring(rev_id,
                                    page_id,
                                    user_id,
                                    v_time,
                                    page_title,
                                    "vote",
                                    connection,
                                    DB_PREFIX)
     
## Add a colored text request
def addTextRequest(rev_id, page_id, user_id, rev_time, page_title):
  global DB_PREFIX
  global connection

  server_utils.mark_for_coloring(rev_id, 
                                 page_id, 
                                 user_id, 
                                 rev_time, 
                                 page_title,
                                 "coloring",
                                 connection,
                                 DB_PREFIX)

## Add a edit
def addEdit(rev_id, page_id, user_id, e_time, page_title):
  global DB_PREFIX
  global connection

  server_utils.mark_for_coloring(rev_id,
                                 page_id,
                                 user_id,
                                 e_time,
                                 page_title,
                                 "edit",
                                 connection,
                                 DB_PREFIX)

try:
  opts, args = getopt.gnu_getopt(sys.argv[1:], "h", ["help"])
except getopt.GetoptError:
  # print help information and exit:
  usage()
  sys.exit(2)

# check the args
for a in args:
  dumps.append(a)
for o, a in opts:
  if o in ("-h", "--help"):
    usage()
    sys.exit(2)
    
## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

DB_PREFIX = ini_config.get('db', 'prefix')

print "Deleting all entries from the DB......."

curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_global")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_page")     
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_vote")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_revision")
curs.execute("delete from "+ini_config.get('db', 'prefix')+\
             "wikitrust_colored_markup")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_user")
curs.execute("delete from "+ini_config.get('db', 'prefix')+\
             "wikitrust_missing_revs")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_sigs")

connection.commit()

print "DB Cleared\n"

## Now, do some events
event_functions = {VOTE: addVote,
                   TEXT: addTextRequest,
                   EDIT: addEdit}
                   

for event in events:
  event_functions.get(event.get('type'))(
      event.get('rev_id'),
      event.get('page_id'),
      event.get('user_id'),
      event.get('v_time'),
      event.get('page_title'),
      )
      
