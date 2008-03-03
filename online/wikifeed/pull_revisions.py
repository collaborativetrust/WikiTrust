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

# Pulls revision ids from the wikipedia api
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
WIKI_BASE = "http://en.wikipedia.org/w/api.php"
NUM_TO_PULL = 200
EPSILON_TO_STOP_AT = 100
LOCK_FILE = "/tmp/mw_revision_pull"
SLEEP_TIME_SEC = 1
MAX_TIMES_TRY = 10
BASE_DIR = "/home/ipye/dev/wikifeed/"                                                     
INI_FILE = BASE_DIR + "pull_revision.ini"

## re to pull out the xml header
patern = re.compile('<\?xml version="1.0" encoding="utf-8"\?>')

revs_added = 0

## parser callbacks
def start_element(name, attrs):
  global revs_added
  if name == "rc":
    revid = int(attrs["revid"])
    if revid > 0: 
      curs.execute("SELECT revision FROM trust_revision_q WHERE revision = %s", (revid))
      data = curs.fetchall()
      if len(data) <= 0:
        curs.execute("INSERT INTO trust_revision_q (revision) VALUES (%s)", (revid))
        revs_added += 1

## Usage method
def usage():
  print "Usage: python [-h, --help, -v] pull_revisions.py"

## non-const globals
verbose = False
args = ""
last_ux_timestamp = 0
num_revs = 0

## TODO -> convert this into a UTC timestamp for portability
sql_get_last_timestamp = "SELECT UNIX_TIMESTAMP(addedon) FROM trust_revision_q \
                          ORDER BY addedon DESC LIMIT 1"

def pull_revs():                                                                                            
  global last_ux_timestamp
  global parser
  global patern

  contents = ""
  clean_content = ""
  ## First, get the timestamp we left off at                                                           
  curs.execute (sql_get_last_timestamp)
  data = curs.fetchall()                                                                                    
  for row in range(len(data)):   
    last_ux_timestamp = data[row][0]   
  ## Now, pull the revision ids from the api                                              
  api_data = urllib.urlencode({"action" : "query",      
                               "list" : "recentchanges",                                   
                               "rclimit" : NUM_TO_PULL,                                    
                               "format" : "xml",                                           
                               "reprop" : "revs",                                          
                               "restart" : last_ux_timestamp                               
                             })  

  # the ? is for a GET request                                                            
  request = urllib2.Request( WIKI_BASE + "?" + api_data )
  request.add_header('Accept-encoding', 'gzip')  # all the content to be sent back in gzip'd form
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
    except StandardError, inst:
      num_times_tried_http += 1
      time.sleep(SLEEP_TIME_SEC)
      if verbose:
        print inst
        print "Error fetching content"

  ## Parse the results, and put the contents in the db                                    
  ## Callback functions defined above handle the rest                                     
  ## remove the xml line
  clean_content = patern.sub( '', clean_content, count=1)
  try:
    parser.Parse(clean_content)  
  except StandardError, inst:
    if verbose:
      print inst
      print "Error parsing content"
    return -1

  return revs_added

## init expat
parser = xml.parsers.expat.ParserCreate()
parser.StartElementHandler = start_element
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
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
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

# now, keep pulling revs untill there are less than epsilon pulled
while (pull_revs() > EPSILON_TO_STOP_AT):
  num_revs += 1
  time.sleep(SLEEP_TIME_SEC)

## close off the root element
parser.Parse("</START_XML_LIST>", True) ## keep the xml well formed

# remove the lock file
os.remove(LOCK_FILE)

## and report
if verbose:  
  print "Pulling From: " + str(last_ux_timestamp) + ". Added " + str(revs_added) + " revisions"
