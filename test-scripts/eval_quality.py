#!/usr/bin/python

"""
Copyright (c) Luca de Alfaro
All rights reserved.

Authors: Luca de Alfaro

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
import csv

## const globals
DB_ENGINE = "mysql"
BASE_DIR = "./"   
INI_FILE = BASE_DIR + "db_access_data.ini"

## Usage method
def usage():
  print "Usage: eval_quality.py revision_file.csv"


## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

# Scans the csv file for input information.

if len(sys.argv) != 2:
  usage();
  sys.exit(-1)
file_name = sys.argv[1]
csv_f = csv.reader(open (file_name, "r"), delimiter = ',', quotechar = '"')
row_idx = 0
for row in csv_f:
  row_idx += 1
  if row_idx == 1:
    print 'Title,Revision,N.Judges,Judge Confidence,Quality,Min Quality,Delta,h[0],h[1],h[2],h[3],Histogram,Revision,N.Judges,Judge Confidence,Quality,Min Quality,Delta,h[0],h[1],h[2],h[3],Histogram'
  else:
    print '"' + row[0] + '",',
    for rev_spec in row[1:]:
      rev_id = int (rev_spec)
      print '"', rev_id, '",',
      curs.execute("select quality_info from " + ini_config.get('db', 'prefix') + "wikitrust_revision where revision_id = %s",
                   (rev_id,))
      t = curs.fetchone()
      if t != None:
        s = t[0]
        # N. edit judges
        i = s.find("n_edit_judges ") + len("n_edit_judges ")
        j = s[i:].find(")")
        n_edit_judges = int (s[i:i+j])
        # Judge confidence
        i = s.find("judge_weight ") + len("judge_weight ")
        j = s[i:].find(")")
        judge_weight = float (s[i:i+j])
        # Total edit quality
        i = s.find("total_edit_quality ") + len("total_edit_quality ")
        j = s[i:].find(")")
        total_quality = float (s[i:i+j])
        # Min quality
        i = s.find("min_edit_quality ") + len("min_edit_quality ")
        j = s[i:].find(")")
        min_quality = float (s[i:i+j])
        # Delta
        i = s.find("delta ") + len("delta ")
        j = s[i:].find(")")
        delta = float (s[i:i+j])
        # Histogram
        i = s.find("word_trust_histogram(") + len("word_trust_histogram(")
        j = s[i:].find(")")
        hist = s[i:i+j].split(" ")
        # produce output
        print '"', n_edit_judges, '","', judge_weight, '","', 
        if judge_weight > 0.01:
          print total_quality / judge_weight, '","',
        else: 
          print '0","',
        print min_quality, '","', delta, '","',
        print hist[0], '","', hist[1], '","', hist[2], '","', hist[3], '","',
        print " ".join(hist), '",',
    print

    
    
