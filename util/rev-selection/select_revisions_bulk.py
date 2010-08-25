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
from revision_selection import select_best_revisions

def usage():
    print "cat <datafile> | ./select_revisions_bulk.py 3 > outfile.csv"
    print "where '3' is the number of revisions to print for each page_id"

if len(sys.argv) != 2:
    usage()
    sys.exit(2)

num_winners = int(sys.argv[1])

## csv file initialization
fieldnames = ("Page_id", "Revision_id", "Quality", "Risk",
              "Date", "Days ago", "Revisions Ago", "Rank", "Url")

writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames,
                        quoting=csv.QUOTE_NONNUMERIC)
headers = dict( (n,n) for n in fieldnames )
writer.writerow(headers)

def write_row(out):
    row = {}
    for f in fieldnames:
        row[f] = out[f]
    writer.writerow(row)

for l in sys.stdin:
    try:
        page_id = int(l.split()[-1])
        revision_list = select_best_revisions(page_id, num_winners)
        rank = 0;
        out = {}
        for (page_id, rev_id, disc_q, q, r, d, a, n) in revision_list:
            rank += 1
            out["Page_id"] = page_id
            out["Revision_id"] = rev_id
            out["Quality"] = q
            out["Risk"] = r
            out["Date"] = d
            out["Days ago"] = a
            out["Revisions Ago"] = n
            out["Rank"] = rank
            out["Url"] = ("http://en.wikipedia.org/w/index.php?oldid=" +
                          str(rev_id) + "&trust")
            write_row(out)
    except ValueError:
        pass
    
