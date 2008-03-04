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

# Pulls user ids from the wikipedia 
import MySQLdb
import sys
import getopt
import urllib
import urllib2
import gzip
import os
import re
import time
import StringIO
import xml.parsers.expat
import ConfigParser

## Ian Pye <ipye@cs.ucsc.edu>

## const globals
WIKI_BASE = "http://en.wikipedia.org/w/index.php"
NUM_TO_PULL = 5
EPSILON_TO_STOP_AT = 100
LOCK_FILE = "/tmp/mw_user_pull"
SLEEP_TIME_SEC = 1
MAX_TIMES_TRY = 1
BASE_DIR = "/home/ipye/dev/wikifeed/"
INI_FILE = BASE_DIR + "pull_revision.ini"

## re to pull out the xml header
patern = re.compile('\<mediawiki(.)*?xml:lang="en"\>')

## repalce ' ' with '_'
user_patern = re.compile(' ')

users_added = 0
last_users_added = []

def add_user():
  global current_user_id
  global current_user
  global users_added
  global user_text
  global last_users_added
  
  user_uni = current_user.encode('utf-8')
  if current_user in last_users_added:
    return

  if user_uni != user_text.encode('utf-8'):
    if verbose:
      print "error -- " + user_uni + " does not match " + user_text
    return 
  
  if verbose:
    print "adding " + user_uni

  user_id = int(current_user_id)
  if user_id > 0:                                                                                           
    curs.execute("SELECT trust_user FROM trust_users WHERE trust_user_text = %s", (user_uni))                    
    data = curs.fetchall()                                                                                
    if len(data) <= 0:                                                                                    
      curs.execute("INSERT INTO trust_users (trust_user, trust_user_text) VALUES (%s, %s)", (user_id, user_uni))
      users_added += 1   
    ## now, update all the revision rows for this user
    curs.execute("UPDATE trust_revision SET rev_user = %s WHERE rev_user_text = %s", (user_id, user_uni))

    ## mark that we have already looked at this guy
    last_users_added.append(user_uni)

## parser callbacks
def start_element(name, attrs):
  global in_contrib
  global in_user
  global in_user_id
  if name == "contributor":
    in_contrib = True
  elif name == "username" and in_contrib:
    in_user = True
  elif name == "id" and in_contrib:
    in_user_id = True

def end_element(name):
  global in_contrib
  global in_user
  global in_user_id
  global current_user
  global current_user_id

  if name == "contributor":
    in_contrib = False
    add_user()
    current_user = ""
    current_user_id = ""
  elif name == "username" and in_contrib:
    in_user = False
  elif name == "id" and in_contrib:
    in_user_id = False

def char_data(data):
  global in_user
  global in_user_id
  global current_user
  global current_user_id
  if in_user:
    current_user += data
  elif in_user_id:
    current_user_id += data

## Usage method
def usage():
  print "Usage: python [-h, --help, -v] pull_users.py"

## non-const globals
in_contrib = False 
in_user = False
in_user_id = False
current_user = ""
current_user_id = ""
verbose = False
args = ""
user_text = ""
num_users = 0

sql_get_users = "select rev_user_text from trust_revision where \
                 rev_user = 0 and rev_user_text NOT REGEXP \
                 '^[[:digit:]]*.[[:digit:]]*.[[:digit:]]*.[[:digit:]]*$'" 


def pull_revs():                                                                                            
  global parser
  global patern
  global user_patern
  global user_text
  global last_users_added

  user_text = ""
  contents = ""
  clean_content = ""
  api_data = ""
  tmp_user = ""

  ## First, get the timestamp we left off at                                                           
  curs.execute (sql_get_users)
  data = curs.fetchall()                                                                                    
  for row in range(len(data)):   
    user_text = data[row][0]   

    ## replace spaces with '_'
    tmp_user = user_patern.sub( '_', user_text )
    api_data = urllib.urlencode({'title'  : 'Special:Export',
                                 'pages'  : 'User:'+tmp_user,
                                 'offset' : '1',
                                 'limit'  : '5',
                                 'action' : 'submit'
                                })

    ## Now, pull the user ids from the api                                              
    request = urllib2.Request( WIKI_BASE , api_data ) ## post request
    request.add_header('Accept-encoding', 'gzip')  # all the content to be sent back in gzip'd form
    request.add_header('User-Agent', 'Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.12) Gecko/20080201 Firefox/2.0.0.12')
    request.add_header('Accept', 'text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5')
    opener = urllib2.build_opener()
    num_times_tried_http = 0

    while num_times_tried_http < MAX_TIMES_TRY:
      try:
        f = opener.open(request)
        contents = f.read()                                                                     

        clean_content = contents
        ## check to see if the stream is encrypted or not
        if f.headers.get('Content-Encoding') == "gzip":
          # now, the contents are in gzip'd form
          compressedstream = StringIO.StringIO(contents)
          gzipper = gzip.GzipFile(fileobj=compressedstream)
          clean_content = gzipper.read() # and now they are not

        f.close()                                                                               
        break
      except:                                                                                              
        num_times_tried_http += 1
        time.sleep(SLEEP_TIME_SEC)
        if verbose:
          print "Error fetching content: " + WIKI_BASE + api_data

    ## Parse the results, and put the contents in the db                                    
    ## Callback functions defined above handle the rest                                     
    ## remove the xml line
    clean_content = patern.sub( '<mediawiki>', clean_content, count=1)
    try:
      parser.Parse(clean_content)  
      last_users_added = []
    except xml.parsers.expat.ExpatError, inst : 
      if verbose:
        print inst
        print "Error parsing content"
      return -1

  return users_added

## init expat
parser = xml.parsers.expat.ParserCreate()
parser.buffer_text = True                                                                                   
parser.StartElementHandler = start_element
parser.EndElementHandler = end_element                                                                      
parser.CharacterDataHandler = char_data 
parser.Parse("""<?xml version="1.0" encoding="utf-8"?><START_XML_LIST>""") ## have a single root element

try:
  opts, args = getopt.gnu_getopt(sys.argv[1:], "hv", ["help"])
except getopt.GetoptError:
  # print help information and exit:
  usage()
  sys.exit(2)
for a in args:
  args = a
for o, a in opts:
  if o in ("-h", "--help"):
    usage()
    sys.exit(2)
  if o in ("-v"):
    verbose = True

## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'), \
    user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

## make sure that there is not another process going at the same time
if os.path.exists(LOCK_FILE):
  if verbose:
    print "Error: Lock File exists -- exiting now"
  sys.exit(-2)
else:
  lock_f=open(LOCK_FILE, 'w')
  lock_f.write("0")
  lock_f.close()

# now, pull users
pull_revs()

## close off the root element
parser.Parse("</START_XML_LIST>", True) ## keep the xml well formed

# remove the lock file
os.remove(LOCK_FILE)

## and report
if verbose:  
  print "Added " + str(users_added) + " users"
