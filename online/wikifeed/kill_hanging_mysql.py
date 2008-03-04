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

# Hack to get arround MySQL bug 14070
# Polls the mysql server, and kills any long running "Copying to tmp table on
# disk" threads
# Otherwise, these just linger -- never completing, but sucking up CPU

import MySQLdb
import sys
import getopt
import os
import re
import StringIO
import ConfigParser

## Ian Pye <ipye@cs.ucsc.edu>

## const globals
BASE_DIR = "/home/ipye/dev/wikifeed/"
INI_FILE = BASE_DIR + "pull_revision.ini"
KILL_TASK_DESCRIPTION = "Copying to tmp table on disk"
DESCRIPTION_INDEX = 6
THREAD_ID_INDEX = 0
TIME_INDEX = 5
KILL_TIME = 4000 ## ~1 hour

## Usage method
def usage():
  print "Usage: python [-h, --help, -v] kill_hanging_mysql.py"

## non-const globals
verbose = False
args = ""

sql_get_threads = "SHOW PROCESSLIST"
sql_kill_thread = "KILL QUERY %s"

def kill_threads():                                                                                            
  global verbose

  num_killed = 0
  ## First, get the threads                                                           
  curs.execute (sql_get_threads)
  data = curs.fetchall()                                                                                    
  for row in range(len(data)):   
    if data[row][DESCRIPTION_INDEX] == KILL_TASK_DESCRIPTION:
      if verbose:
        print str(data[row][THREAD_ID_INDEX]) + " - " + data[row][DESCRIPTION_INDEX]
      curs.execute(sql_kill_thread, (data[row][THREAD_ID_INDEX]))  
      num_killed += 1
    elif data[row][TIME_INDEX] > KILL_TIME:
      if verbose:
        print str(data[row][THREAD_ID_INDEX]) + " - " + data[row][TIME_INDEX]
      curs.execute(sql_kill_thread, (data[row][THREAD_ID_INDEX]))
      num_killed += 1
  return num_killed

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
connection = MySQLdb.connect(host=ini_config.get('root', 'host'),
user=ini_config.get('root', 'user'), passwd=ini_config.get('root', 'pass') \
    , db=ini_config.get('root', 'db') )
curs = connection.cursor()

num_killed = kill_threads()

## and report
if verbose:  
  print "killed " + str(num_killed) + " threads."
  
