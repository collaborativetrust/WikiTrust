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

import sys
import csv
from revision_selection import select_recent_spam

def usage():
    print "./find_recent_vandalism.py <start_page_id>"

if len(sys.argv) != 2:
    usage()
    sys.exit(2)

start_page_id = int(sys.argv[1])

## csv file initialization
fieldnames = ("Page_id", "Revision_id", "Date", "Title", "Url")

writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames,
                        quoting=csv.QUOTE_NONNUMERIC)
headers = dict( (n,n) for n in fieldnames )
writer.writerow(headers)

def write_row(out):
    row = {}
    for f in fieldnames:
        row[f] = out[f]
    writer.writerow(row)

page_id = start_page_id
while True:
    page_id += 1
    rl = select_recent_spam(page_id)
    for r in rl:
        (title, rev_id, rev_date) = r
        out = {}
        out["Page_id"] = page_id
        out["Revision_id"] = rev_id
        out["Date"] = rev_date
        out["Title"] = title
        out["Url"] = ("http://en.wikipedia.org/w/index.php?oldid=" +
                      str(rev_id) + "&trust")
        write_row(out)
