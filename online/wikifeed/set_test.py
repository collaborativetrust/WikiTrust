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

## sets up a test run
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

## const globals
MW_DUMPER = "java -Xmx600M -server -jar mwdumper.jar --format=sql:1.5"
DB_ENGINE = "mysql"
BASE_DIR = "./"   
LOG_NAME = BASE_DIR + "online_eval.log"
INI_FILE = BASE_DIR + "online_feed_test.ini"
RUN_TEST_FEED = BASE_DIR + "run_test_feed.sh"
NUM_TO_PULL = 10

revs_added = 0

## Usage method
def usage():
  print "Usage: python set_test.py [-h, --help, -v ] \
                        \n--use_dump dump_file.xml"


## non-const globals
verbose = False
args = ""
num_revs = 0
use_dump = ""

def set_test():                                                                                            
        
  global use_dump
  global revs_added
  global ini_config
  revs = [];
  pages = [];

  # Switch to the pull db
  connection.select_db(ini_config.get('db', 'db'))

  # clear out the pull db
  curs.execute("delete from text")
  curs.execute("delete from page")
  curs.execute("delete from revision")
  connection.commit()

  # Load the xml file into the sender db
  os.system(MW_DUMPER + " " + use_dump + " | " + DB_ENGINE + " -u " +
      ini_config.get('db', 'user') + " -p" + ini_config.get('db', 'pass') + " " +
      ini_config.get('db', 'db'))

  ## Get the rev id's we are going to work with                                                          
  curs.execute ("select rev_id, rev_page from revision")
  data = curs.fetchall()                                                                                    
  for row in range(len(data)):   
    revs.append(data[row][0])
    pages.append(data[row][1])

  connection.select_db(ini_config.get('db', 'db'))

  ## Now fill up the revs
  for rev in revs:
    #curs.execute("insert into trust_revision_q (revision) values ("+str(rev)+")")
    revs_added += 1

  return revs_added

try:
  opts, args = getopt.gnu_getopt(sys.argv[1:], "hv", ["help", "use_dump="])
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
  if o in ("--use_dump"):
    use_dump = a
  if o in ("-v"):
    verbose = True

if use_dump == "" or eval_wiki == "":
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

set_test() 

if verbose:
  print "Created a new test instance with " + str(revs_added) + " revisions."
