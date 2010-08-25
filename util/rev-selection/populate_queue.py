#!/usr/bin/python

"""
Copyright (c) 2010 Luca de Alfaro.
All rights reserved.

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

import MySQLdb
import sys
import os
import ConfigParser

## const globals
DB_ENGINE = "mysql"
BASE_DIR = "./"   
INI_FILE = BASE_DIR + "db_access_data.ini"

## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
                             user=ini_config.get('db', 'user'), 
                             passwd=ini_config.get('db', 'pass'),
                             db=ini_config.get('db', 'db') )
curs = connection.cursor()

# Populates the queue
for l in sys.stdin:
  ll = l.split('\t')
  page_id = int(ll[1].strip())
  title = ll[0]
  if True:
    curs.execute("insert ignore into " + ini_config.get('db', 'prefix') + 
                 "wikitrust_queue (title, page_id) values (%s, %s)", ("", page_id, ))
    curs.execute("commit")
