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
  print "Usage: python truncate_wikitrust_tables.py"


## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
user=ini_config.get('db', 'user'), passwd=ini_config.get('db', 'pass') \
    , db=ini_config.get('db', 'db') )
curs = connection.cursor()

curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_global")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_page")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_vote")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_revision")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_blob")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_queue")
curs.execute("delete from "+ini_config.get('db', 'prefix')+"wikitrust_text_cache")

connection.commit()



    

