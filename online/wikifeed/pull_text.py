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

# Pulls full info of revisions from the wikipedia api and puts it in a database
import MySQLdb
import sys
import getopt
import urllib, urllib2
import signal, os
import StringIO
import gzip
import time
import re
import xml.parsers.expat

## Ian Pye <ipye@cs.ucsc.edu>

# const globals
USER = "wikiuser"
PASS = "wikiword"
DB = "wikitest"

WIKI_BASE = "http://en.wikipedia.org/w/api.php"
REVS_TO_PULL = 20
LOCK_FILE = "/tmp/mw_test_pull"
SLEEP_TIME_SEC = 1
MAX_TIMES_TRY = 10 ## how many times to try downloading content before giving up

# FileCache
FILE_CACHE_ROOT = "/var/www/rev_text"
CHARS_PER_CACHE_FILE = 3
FILE_CACHE_DIR_SEP = "/"
FILE_CACHE_PAGE_SIZE = 12
FILE_CACHE_REV_SIZE = 12
FILE_CACHE_EXTENSION = ".txt"
REV_FLAGS = "utf-8,fs-cache"

## init the DB
connection = MySQLdb.connect(host="localhost",
user=USER, passwd=PASS, db=DB )
curs = connection.cursor()

revs_added = 0

## re to pull out the xml header
patern = re.compile('<\?xml version="1.0" encoding="utf-8"\?>')

## signal handler
def int_handler(signum, frame):
  global keep_going
  global verbose
  if verbose:
    print "Caught Stop Signal -- Shutting down after current run."
  keep_going = False

##, and setup the sig handler
signal.signal(signal.SIGINT, int_handler)

## find out where to put the text
def get_page_dir(page_id):
  page_id = page_id.rjust(FILE_CACHE_PAGE_SIZE,'0')
  page_dir = FILE_CACHE_ROOT + FILE_CACHE_DIR_SEP
  if not os.path.exists(page_dir):
    return ""
  page_dir += page_id[0:3] + FILE_CACHE_DIR_SEP
  if not os.path.exists(page_dir):
    os.mkdir(page_dir)
  page_dir += page_id[3:6] + FILE_CACHE_DIR_SEP
  if not os.path.exists(page_dir):
      os.mkdir(page_dir)
  page_dir += page_id[6:9] + FILE_CACHE_DIR_SEP
  if not os.path.exists(page_dir):
      os.mkdir(page_dir)
  page_dir += page_id[9:12] + FILE_CACHE_DIR_SEP
  if not os.path.exists(page_dir):
      os.mkdir(page_dir)
  return page_dir

## Put the text in the appropriate place
def put_file_content(page_dir, revision_id, contents):
  revision_id = revision_id.rjust(FILE_CACHE_REV_SIZE,'0')
  file_name = page_dir + revision_id + FILE_CACHE_EXTENSION
  f=open(file_name, 'w')
  f.write(contents.encode('utf-8'))
  f.close()
  return 

## parser callbacks -- TOTO Fix user ID
def save_data():
  global REV_FLAGS
  global current_rev_id
  global current_page_id
  global current_revs
  global page_data
  global num_pages
  global num_revs
  global verbose

  if verbose:
    print "Saving page " + current_page_id + ", with " + str(len(current_revs)) + " Revisions.";
  ## first, see if we need to add the page
  curs.execute("SELECT page_id FROM trust_page WHERE page_id = %s", (page_data["pageid"]))
  data = curs.fetchall()
  if len(data) <= 0:
    num_pages += 1
    curs.execute("INSERT INTO trust_page (page_id, page_namespace, page_title, \
                  page_touched, page_latest, page_counter, page_len) \
                  VALUES \
                  (%s, %s, %s, %s, %s, %s, %s) ", \
                  (page_data["pageid"], page_data["ns"], page_data["title"].encode('utf-8') \
                  , page_data["touched"][:19] \
                  , page_data["lastrevid"], page_data["counter"], page_data["length"] ))
  
  ## now, insert the revs, as needed
  for rev in current_revs:
    curs.execute("SELECT rev_id FROM trust_revision WHERE rev_id = %s", (rev["revid"]))
    data = curs.fetchall()
    if len(data) <= 0:
      num_revs += 1
      comment=""
      user = ""
      timestamp = "1970-01-01 00:00:00"
      size = "0"
      userid = "0"
      if rev.has_key("size"):
        size = rev["size"]
      if rev.has_key("timestamp"): 
        timestamp = rev["timestamp"]
      if rev.has_key("user"):  
        user = rev["user"] 
      if rev.has_key("comment"):
        comment=rev["comment"]
      curs.execute("INSERT INTO trust_revision (rev_id, rev_page, rev_text_id, rev_comment, rev_user \
                    ,rev_user_text,rev_timestamp,rev_len) \
                    VALUES \
                    (%s, %s, %s, %s, %s, %s, %s, %s)", \
                    (rev["revid"], current_page_id,  rev["revid"] \
                    , comment.encode('utf-8'), userid, user.encode('utf-8'), \
                    timestamp[:19], 
                    size) \
                    )

      ## finally, insert the text
      curs.execute("INSERT INTO trust_text (old_id, old_text, old_flags) VALUES \
                    (%s, %s, %s)",\
                    (rev["revid"], "", REV_FLAGS))
      ## and send the text to the FS
      page_dir = get_page_dir(current_page_id);
      if not page_dir == "":
        put_file_content(page_dir, rev["revid"], rev["content"]);

      ## last but not least, mark the revision as being processed
      curs.execute("UPDATE trust_revision_q SET status = 'downloaded' WHERE revision = %s",
                  rev["revid"])
  return

def handle_page(attrs):
  global current_page_id
  global page_data
  current_page_id = {}
  for k, v in attrs.items():
    page_data[k] = v
    if k == "pageid":
      current_page_id = v

def handle_revisions(attrs):
  global current_rev_id
  global current_rev_data
  current_rev_data = {}
  current_rev_data["content"] = ""
  for k, v in attrs.items():
    current_rev_data[k] = v
    if k == "revid":
      current_rev_id = v

def start_element(name, attrs):
  if name == "page":
    handle_page(attrs)
  elif name == "rev":
    handle_revisions(attrs)

def end_element(name):
  global current_rev_data
  global current_rev_id
  global current_page_id
  global current_revs 
  global page_data 

  if name == "rev":
    current_revs.append(current_rev_data)
    current_rev_id = 0
  elif name == "page":
    save_data()  ## save the data, and then clear out everything for the next pass
    current_page_id = 0
    current_revs = []
    current_rev_data = {}
    page_data = {}

def char_data(data):
  global current_rev_data
  global current_rev_id
  if current_rev_id > 0:
    current_rev_data["content"] += data

## Usage method
def usage():
  print "Usage: python [-h, --help, -v, -d] pull_text.py"

## non-const globals
keep_going = False
verbose = False
deamon = False
args = ""
num_revs = 0
num_pages = 0
current_page_id = 0
current_rev_id = 0
page_data = { }
current_revs = []            
current_rev_data = { }

## sql to get the revids to work on
sql_get_revs_to_pull = "SELECT revision FROM trust_revision_q \
                        WHERE NOT processedon  \
                        AND status = 'added' \
                        ORDER BY addedon LIMIT " + str(REVS_TO_PULL)

def pull_text():                                                                                            
  global num_revs
  global parser
  global patern
  clean_content = "" 
  rev_ids = []
  ## First, get the timestamp we left off at                                                           
  curs.execute (sql_get_revs_to_pull)
  data = curs.fetchall()                                                                                    
  for row in range(len(data)):   
    rev_ids.append( data[row][0] )

  ## first, mark the revs as being processed
  curs.executemany("UPDATE trust_revision_q SET status = 'downloading' WHERE revision = %s",
                  rev_ids)

  ## Now, pull the revision ids from the api                                              
  api_data = urllib.urlencode({"action" : "query",      
                               "prop" : "revisions|info",
                               "format" : "xml",
                               "rvprop" : "ids|flags|timestamp|user|size|comment|content",
                               "revids" : "|".join(["%s" % (v) for v in rev_ids ])
                             })  

  # the ? is for a GET request                                                            
  request = urllib2.Request( WIKI_BASE + "?" + api_data )
  request.add_header('Accept-encoding', 'gzip')  # all the content to be sent back in gzip'd form
  opener = urllib2.build_opener()
    
  # keep going untill we give up, or get some content  
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

      f.close() ## in any event, close the file
      break
    except:
      num_times_tried_http += 1
      ## question -- should I raise the exception again here?
      if num_times_tried_http >= MAX_TIMES_TRY:
        if verbose:
          print "Giving up fetching with error"
          return -1
      if verbose:
        print "Error fetching content -- trying again"
      time.sleep(SLEEP_TIME_SEC)

  ## Parse the results, and put the contents in the db                                    
  ## Callback functions defined above handle the rest                                     
  clean_content = patern.sub( '', clean_content, count=1)  

  try:
    parser.Parse(clean_content)  
  except:
    if verbose:
      print "Error parsing content"
    return -1
  return num_revs

## init expat
parser = xml.parsers.expat.ParserCreate()
parser.buffer_text = True
parser.StartElementHandler = start_element
parser.EndElementHandler = end_element                                                
parser.CharacterDataHandler = char_data
parser.Parse("""<?xml version="1.0" encoding="utf-8"?><START_XML_LIST>""") ## have a single root element

try:
  opts, args = getopt.gnu_getopt(sys.argv[1:], "hvd", ["help"])
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
  if o in ("-d"):
    deamon = True
    keep_going = True

## make sure that we are the only pull_text running currently
if os.path.exists(LOCK_FILE):
  print "Error: Lock File exists -- exiting now"
  sys.exit(-2)
else:
   lock_f=open(LOCK_FILE, 'w')
   lock_f.write("0")
   lock_f.close()

## keep going, unless we are told to stop, or we are not in deamon mode
pull_text() 
while keep_going:
  time.sleep(SLEEP_TIME_SEC)
  pull_text()

## remove the lock file
os.remove(LOCK_FILE)

## close off the root element
parser.Parse("</START_XML_LIST>", True) ## keep the xml well formed     

## and report
if verbose:  
  print "Added " + str(num_pages)+ " pages, " + str(num_revs) + " revisions"
