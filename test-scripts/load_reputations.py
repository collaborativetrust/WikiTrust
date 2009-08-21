#!/usr/bin/python

"""
Copyright (c) 2007-2008 The Regents of the University of California
All rights reserved.

Authors: Ian Pye, Luca de Alfaro

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
import os
import ConfigParser

## const globals
DB_ENGINE = "mysql"
BASE_DIR = "./"   
INI_FILE = BASE_DIR + "db_access_data.ini"

## Usage method
def usage():
  print "Usage: python load_reputations.py [-h, --help, --clear_db, --set_histogram] reputation_file"



## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

## Reads the command-line options.
try:
  opts, args = getopt.gnu_getopt(sys.argv[1:], "h", ["help", "clear_db", "set_histogram"])
except getopt.GetoptError:
  usage()
  sys.exit(2)

rep_files = []
do_clear = False
set_histogram = False
histogram = [0 for i in xrange(10)]
median = 9  # True for large wikis

for a in args:
  rep_files.append(a)
for o, a in opts:
  if o in ("-h", "--help"):
    usage()
    sys.exit(2)
  if o in ("--clear_db"):
    do_clear = True
  if o in ("--set_histogram"):
    set_histogram = True


# Clear out the user reputations if requested.
if do_clear: 
  curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_user")     
  connection.commit()

## Loads the reputations.
for f_name in rep_files:
  f = open (f_name, "r")
  while True:
    l = f.readline ()
    if l == "": break
    l_p = l.split ()
    uid = int (l_p [1])
    rep = float (l_p [4])
    weight = int (l_p [3])
    histogram [weight] += 1
    curs.execute ("insert into " + ini_config.get('db', 'prefix') +
                  "wikitrust_user (user_id, user_rep) values ( %s , %s ) on duplicate key update user_rep = %s", 
                  (uid, rep, rep) )
  connection.commit ()
  f.close ()
  # Writes the histogram if requested.
  if set_histogram:
    curs.execute ("delete from "+ini_config.get('db', 'prefix')+"wikitrust_global")
    curs.execute ("insert into " + ini_config.get('db', 'prefix') +
                  "wikitrust_global (median, rep_0, rep_1, rep_2, rep_3, rep_4, rep_5, rep_6, rep_7, rep_8, rep_9) values ( %s , %s , %s , %s , %s , %s , %s , %s , %s , %s , %s )",
                  (median, 
                   histogram [0], histogram [1], histogram [2], 
                   histogram [3], histogram [4], histogram [5], 
                   histogram [6], histogram [7], histogram [8], 
                   histogram [9]) )
    connection.commit ()

                   

    

