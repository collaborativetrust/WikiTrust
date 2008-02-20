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

from socket import *
from sys import argv, exit
from os import fork
import MySQLdb

## Runs a fork on accept style server, and adds the things it gets to the DB
## Note that all UDP messages must be prefixed by the SEK string

DEFAULT_PORT = 10123
PORT = DEFAULT_PORT

if len (argv) > 1:
  PORT = argv[1]

USER = "wikiuser"
PASS = "wikiword"
DB = "wikitest"

SEK = "sjka48hfkds75QQ:"

def add_to_rev_q(sock, message, client_addr):
  
    if SEK != message[0:len(SEK)]:
      exit()

    try:
      connection = MySQLdb.connect(host="localhost",
          user=USER, passwd=PASS, db=DB )
      curs = connection.cursor()
      curs.execute( """INSERT INTO trust_revision_q (revision) VALUES  \
          (%s)""", message[len(SEK):] )
    except MySQLdb.OperationalError, message:
      errorMessage = "Error %d:\n%s" % (message[0], message[1] )
      print errorMessage
      exit()
    else:  
      exit()

sock = socket(AF_INET, SOCK_DGRAM)
sock.bind(('',int(PORT)))

while 1:    # Run until cancelled
    message, client_addr = sock.recvfrom(256)
    if fork():
        add_to_rev_q(sock, message, client_addr)
