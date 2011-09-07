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
from revision_selection import compute_quality_stats, is_page_ok, get_random_page_id
from revision_selection import get_page_id_from_title

def usage():
    print """cat <datafile> | ./genewiki_stats.py <n_revisions> <multiplier> [detailed_file.csv] > outfile.csv
    where <n_revisions> is the number of revisions to analyze for each article, and 
    <multiplier> is the ratio between the number of non-genewiki and genewiki pages."""

if len(sys.argv) < 3 or len(sys.argv) > 4:
    usage()
    sys.exit(2)

num_revisions = int(sys.argv[1])
multiplier = float(sys.argv[2])
if len(sys.argv) == 4:
    detailed_file = csv.writer(open(sys.argv[3], 'wb'), delimiter=',',
                               escapechar = '\\', quotechar='"', doublequote=False, 
                               quoting=csv.QUOTE_NONNUMERIC)
else:
    detailed_file = None

# Minimum page requirements to avoid redirects and too-short pages.
MIN_PAGE_LENGTH = 200
MIN_PAGE_REVISIONS = 7

# csv file initialization
fieldnames = (
    "Page_id", "Page_title",
    "TAvg_length", "EAvg_length", 
    "TFrac_vandalism", "EFrac_vandalism", 
    "TFrac_neg_qual", "EFrac_neg_qual", 
    "TFrac_reverts", "EFrac_reverts", 
    "Avg_delta", 
    "TAvg_edit_quality", "EAvg_edit_quality",
    "Avg_change_quality",
    "TFrac_untrusted_text", "EFrac_untrusted_text", 
    "TAvg_trust", "EAvg_trust", 
    "TAvg_reputation","EAvg_reputation",
    "Is_genewiki", )

writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames,delimiter=',',
                        escapechar = '\\', quotechar='"', doublequote=False, 
                        quoting=csv.QUOTE_NONNUMERIC)
headers = dict( (n,n) for n in fieldnames )
writer.writerow(headers)

def write_row(out):
    row = {}
    for f in fieldnames:
        row[f] = out.get(f, None)
    writer.writerow(row)
    sys.stdout.flush()

def truncate(x, n):
    k = 10 ** n
    return float(int(x * k)) / float(k)

# We keep track of how many GeneWiki pages we analyze, so that we analyze a similar number
# of other pages.
genewiki_pages_analyzed = set()
first_line = True
for l in sys.stdin:
    try:
        ll = l[:-1].split('\t')
        page_title = ll[1]
        page_id = get_page_id_from_title(page_title)
        if page_id != None and is_page_ok(page_id, MIN_PAGE_REVISIONS, MIN_PAGE_LENGTH):
            genewiki_pages_analyzed.add(page_id)
            out = compute_quality_stats(page_id, num_revisions,
                                        page_title=page_title,
                                        detailed_file=detailed_file,
                                        header_line=first_line)
            first_line = False
            out["Is_genewiki"] = True
            out["Page_id"] = page_id
            write_row(out)
    except ValueError:
        pass
    
# Now analyzes the same number of general pages.
for i in xrange(int(multiplier * len(genewiki_pages_analyzed))):
    while True:
        page_id = get_random_page_id(min_revisions=MIN_PAGE_REVISIONS,
                                     min_length=MIN_PAGE_LENGTH)
        if page_id not in genewiki_pages_analyzed:
            break
    out = compute_quality_stats(page_id, num_revisions,
                                detailed_file=detailed_file)
    out["Is_genewiki"] = False
    out["Page_id"] = page_id
    write_row(out)
