#!/usr/bin/python

"""
Copyright (c) 2010-11 Luca de Alfaro.
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

import calendar
import ConfigParser
import math
import MySQLdb
import os
import random
import sys
import time

## const globals
DB_ENGINE = "mysql"
BASE_DIR = "./"   
INI_FILE = BASE_DIR + "db_access_data.ini"

NUM_REVISION_DISCOUNT = 100
REVISION_AGE_DISCOUNT = 365.0 * 24.0 * 3600.0
AGE_REVISION_CONVERSION = 7.0 * 24.0 * 3600.0

## parse the ini file
ini_config = ConfigParser.ConfigParser()
ini_config.readfp(open(INI_FILE))

## init the DB
connection = MySQLdb.connect(host=ini_config.get('db', 'host'),
                             user=ini_config.get('db', 'user'), 
                             passwd=ini_config.get('db', 'pass'),
                             db=ini_config.get('db', 'db') )
curs = connection.cursor()

## Gets an element from quality_info
def get_element(info, name):
  i = info.index (name) + len(name) + 1
  j = info.index (")", i)
  return info[i:j]

# Given a revisiion id, reads raw revision quality data from the database.
def read_revision_data_from_db(rev_id):
  # Gets data on the rev_id revision from the db.
  curs.execute ("select page_id, time_string, user_id, username, " +
                "quality_info, overall_trust from " + 
                ini_config.get('db', 'prefix') + 
                "wikitrust_revision where revision_id = %s", rev_id )
  row = curs.fetchone ()
  if row == None:
    return None
  out = {}
  out ["Revision_id"] = int(rev_id)
  page_id = row [0]
  out ["Page_id"] = int(page_id)
  cur_timestamp = str (row [1])
  out ["Timestamp"] = cur_timestamp
  user_id = int (row [2])
  out ["User_id"] = int(user_id)
  out ["Username"] = str (row [3])
  quality_info = str(row [4])
  out ["Overall_trust"] = float (row [5])
  out ["N_judges"] = int (get_element (quality_info, "n_edit_judges"))
  judge_weight = float (get_element (quality_info, "judge_weight"))
  out ["Judge_weight"] = float(judge_weight)
  total_edit_quality = float(get_element (quality_info, "total_edit_quality"))
  if judge_weight > 0:
    out ["Avg_quality"] = total_edit_quality / (1.0 * judge_weight)
  else:
    out ["Avg_quality"] = 0.0
  out ["Min_quality"] = float (get_element (quality_info, "min_edit_quality"))
  out ["Total_quality"] = total_edit_quality
  out ["Delta"] = float (get_element (quality_info, "delta"))
  ## Gets the histogram.
  hist = get_element (quality_info, "word_trust_histogram").split(" ")
  for i, v in enumerate(hist):
    h = int (v)
    n = "Hist" + str (i)
    out [n] = h

  ## Gets the data on the user.
  user_reputation = 0.0
  if user_id > 0:
    curs.execute ("select user_rep from " +
                  ini_config.get('db', 'prefix') + 
                  "wikitrust_user where user_id = %s", user_id)
    row = curs.fetchone ()
    if row != None:
      user_reputation = row [0]
  out ["Reputation"] =  user_reputation

  ## Previous revision.

  ## Gets data on the preceding revision (old_rev_id) from the db.
  curs.execute ("select time_string, user_id, username, quality_info from " +
                ini_config.get('db', 'prefix') + 
                "wikitrust_revision where page_id = %s and " +
                "time_string < %s order by time_string desc limit 1", 
                (page_id, cur_timestamp))
  row = curs.fetchone ()
  if row != None:
    # There was a previous revision.
    quality_info = str(row [3])
    hist = get_element (quality_info, "word_trust_histogram").split(" ")
    for i, v in enumerate(hist):
      h = int (v)
      n = "Prev_hist" + str (i)
      out [n] = h
    old_user_id = int (row [1])
    out ["Prev_user_id"] = old_user_id
    out ["Prev_username"] = str (row [2])
    out ["Prev_timestamp"] = row [0]
  else:
    # There is no previous revision. We pretend it is the same one.
    for i in xrange(10):
      m = "Hist" + str(i)
      n = "Prev_hist" + str (i)
      out [n] = out [m]
    old_user_id = user_id
    out ["Prev_user_id"] = old_user_id
    out ["Prev_username"] = out ["Username"]
    out ["Prev_timestamp"] = out ["Timestamp"]
    
  ## Next revision.

  ## Now gets the data on the next revision.
  curs.execute ("select user_id, username, time_string from " +
                ini_config.get('db', 'prefix') + 
                "wikitrust_revision where page_id = %s and " +
                "time_string > %s order by time_string asc limit 1", 
                (page_id, cur_timestamp))
  row = curs.fetchone ()
  if row != None:
    ## Computes some parameters due to the comparison with the following
    ## revision.
    out ["Next_user_id"] = row [0]
    out ["Next_username"] = row [1]
    out ["Next_timestamp"] = row [2]
  else:
    ## Fills in reasonable defaults.
    out ["Next_user_id"] = 0
    out ["Next_username"] = ""
    # Uses the current time as default.
    out ["Next_timestamp"] = time.strftime ("%Y%m%d%H%M%S", time.gmtime ())

  ## Finally, writes the line to the output.
  return out

# Unit test.
if False:
  print read_revision_data_from_db(354988943)

# Decides whether two users are the same.
def are_users_the_same (user_id_1, user_id_2, username_1, username_2):
  if (user_id_1 > 0) and (user_id_2 > 0) and (user_id_1 == user_id_2):
    return True
  elif (user_id_1 == 0) and (user_id_2 == 0) and (username_1 == username_2):
    return True
  return False

# Converts timestamps in seconds.
def time_of_date (s):
  y = int (s[0:4])
  m = int (s[4:6])
  d = int (s[6:8])
  h = int (s[8:10])
  mi = int (s[10:12])
  s = int (s[12:14])
  time_v = (y, m, d, h, mi, s, 0, 0, 0)
  return calendar.timegm(time_v)

# Gets the hours from a timestamp.
def get_hours (s):
   h = float (s[8:10])
   m = float (s[10:12])
   s = float (s[12:14])
   return (h + (m / 60.0) + (s / 3600.0))

# Computes features for a revision.
def compute_features(rev_id):
  row = read_revision_data_from_db(rev_id)
  # Extracts user ids.
  user_id = int (row ["User_id"])
  prev_user_id = int (row ["Prev_user_id"])
  next_user_id = int (row ["Next_user_id"])
  # Anonymous, same.
  row ["Anon"] = (user_id == 0)
  row ["Next_anon"] = (next_user_id == 0)
  row ["Next_same_author"] = are_users_the_same (user_id, next_user_id,
                                                     row ["Username"], 
                                                     row ["Next_username"])
  row ["Prev_same_author"] = are_users_the_same (user_id, prev_user_id,
                                                     row ["Username"], 
                                                     row ["Prev_username"])
  # Time of edits
  prev_time = time_of_date (row ["Prev_timestamp"])
  curr_time = time_of_date (row ["Timestamp"])
  next_time = time_of_date (row ["Next_timestamp"])
  row ["Logtime_prev"] = math.log (1.0 + abs (curr_time - prev_time))
  row ["Logtime_next"] = math.log (1.0 + abs (next_time - curr_time))
  row ["Hour_of_day"]  = get_hours (row ["Timestamp"])
  # Max_dissent is the maximum amount of editors who could have voted
  # with Min_quality, if everybody else voted with +1.
  if row["Min_quality"] < 1.0:
    row ["Max_dissent"] = ((row["Judge_weight"] - row["Total_quality"]) /
                           (1 - row["Min_quality"]))
  else:
    row ["Max_dissent"] = 0.0
  # Max_revert is teh maximum amount of editors who could have voted
  # with quality -1, if everybody else voted with +1.
  row ["Max_revert"] = (row["Judge_weight"] - row["Total_quality"]) / 2.0
  # Computes the length of the revisions.
  prev_length = 0
  curr_length = 0
  for i in xrange(10):
    curr_hist_name = "Hist" + str (i)
    prev_hist_name = "Prev_hist" + str (i)
    prev_length += int (row [prev_hist_name])
    curr_length += int (row [curr_hist_name])
  row ["Curr_length"] = curr_length
  row ["Prev_length"] = prev_length
  if curr_length == 0 or prev_length == 0:
    row ["Risk"] = 1.0
  else:
    row ["Risk"] = (max (row ["Delta"], abs(curr_length - prev_length)) /
                    (1.0 * max (curr_length, prev_length)))
  row ["Log_prev_length"] = math.log (1.0 + prev_length)
  row ["Log_length"] = math.log (1.0 + curr_length)
  # Now produces the histogram quantities.
  for i in xrange(10):
    hist_name = "Hist" + str(i)
    prev_hist_name = "Prev_hist" + str (i)
    p_prev_hist_name = "P_prev_hist" + str (i)
    delta_hist_name = "L_delta_hist" + str (i)
    row [p_prev_hist_name] = float (row [prev_hist_name]) / float (1 + prev_length)
    d = float (row [hist_name]) - float (row [prev_hist_name])
    log_d = 0.0
    if d > 0:
      log_d = math.log (1 + d)
    if d < 0:
      log_d = - math.log (1 - d)
    row [delta_hist_name] = log_d
  return row

# Unit test.
if False:
  row = compute_features(354988943)
  out = []
  for (k, v) in row.iteritems():
    out.append((k, v))
  out.sort()
  for (k, v) in out:
    print k, ":", v

# Classifies a revision, adding the fields Quality and Risk.
def classify(rev_id):
  d = compute_features(rev_id)
  if d == None:
    return None
  # Classifier begin
  score = 0.134
  if d["Min_quality"] < -0.662:
    score += 0.891
    if d["L_delta_hist0"] < 0.347: score += -0.974
    else: score += 0.151
    if d["Max_dissent"] < 0.171: score += -1.329
    else:
      score += 0.086
      if "Next_comment_len" in d:
        if d["Next_comment_len"] < 110.5: score += -0.288
        else: score += 0.169
  else: score += -1.203
  if d["Reputation"] < 0.049: score += 0.358
  else:
    score += -1.012
    if d["P_prev_hist5"] < 0.01: score += 0.482
    else:
      score += -0.376
      if d["Avg_quality"] < 0.156: score += 0.5
      else: score += -2.625
      if d["L_delta_hist2"] > 0.347: score += -0.757
      else: score += 1.193
  if "Logtime_next" in d:
    if d["Logtime_next"] < 2.74: score += 1.188
    else:
      score += 0.045
      if d["Delta"] < 3.741: score += -0.255
      else: score += 0.168
  # Classifier end
  prob_good = 1.0 / (1.0 + math.exp(score))
  d["Quality"] = prob_good
  return d


# Unit test.
if False:
  print classify(354988943)

# How much to discount an old revision
def compute_discount(inv_discount_base, num_past_revisions, revision_age):
  d = (inv_discount_base * 1.0) / (inv_discount_base + num_past_revisions + (1.0 * revision_age) / AGE_REVISION_CONVERSION)
  # print "Discount: n=", num_past_revisions, "age=", revision_age, "discount=", d
  return d

# Selects the best revision of a page.
# Returns a list of tuples, in decreasing order of quality, with:
# page_id, revision_id, discounted_quality, quality, risk, revision_date,
# revision_age, number_of_revisions_ago.
def select_best_revisions(page_id, num_winners_to_return, inv_discount_base):
  # Reads the set of past revisions.  We read at most 100 of them.
  curs.execute("select revision_id from " +
               ini_config.get('db', 'prefix') + 
               "wikitrust_revision where page_id = %s order by time_string desc limit 100",
               (page_id, ))
  rev_id_list = curs.fetchall()
  selection = []
  num_past_revisions = 0.0
  current_time = time_of_date(time.strftime ("%Y%m%d%H%M%S", time.gmtime ()))
  for rev_id_s in rev_id_list:
    num_past_revisions += 1
    rev_id = int(rev_id_s[0])
    d = classify(rev_id)
    revision_age = current_time - time_of_date(d["Timestamp"])
    quality = d["Quality"]
    risk = d["Risk"] 
    discount = compute_discount(inv_discount_base, num_past_revisions, revision_age)
    # The Forced is the difference in discounts
    prev_revision_age = current_time - time_of_date(d["Prev_timestamp"])
    prev_discount = compute_discount(inv_discount_base, num_past_revisions + 1, prev_revision_age)
    forced = (discount - prev_discount)
    discounted_quality = quality * discount
    selection.append((discounted_quality, 1.0 - risk, rev_id, d["Timestamp"], quality, risk, forced, discount,
                      revision_age / (24.0 * 3600.0), int(num_past_revisions)))
    selection.sort(reverse=True)
    # If there is no hope of entering the top-N, stops.
    if len(selection) >= num_winners_to_return:
      threshold = selection[num_winners_to_return - 1][0]
      if discount < threshold:
        break
  # Ok, now we have a good selection. Returns the top 10.
  short_list = selection[:num_winners_to_return]
  result = [(page_id, rev_id, dq, q, r, f, t, d, a, m)
            for (dq, _, rev_id, t, q, r, f, d, a, m) in short_list]
  return result

# Unit test
if False:
  short_list = select_best_revisions(1010, 10, 200) # (16965534)
  for (page_id, rev_id, dq, q, r, t, a, m) in short_list:
    print "%d %d %5.4f %5.4f %5.4f %s %5.2f %2d" % (page_id, rev_id, dq, q, r, t, a, m)

# If the latest revision of a page is judged to be spam, returns it.
# Otherwise, returns Null.
def select_recent_spam(page_id):
  ret = []
  current_time = time_of_date(time.strftime ("%Y%m%d%H%M%S", time.gmtime ()))
  # Reads the title.
  curs.execute("select page_title from " +
               ini_config.get('db', 'prefix') +
               'wikitrust_page where page_id = %s', (page_id,))
  title = curs.fetchone()
  if title != None and not (':' in title):
    # Ok, it is a proper page.
    # Reads the last revision.
    curs.execute("select revision_id, time_string from " +
                 ini_config.get('db', 'prefix') + 
                 "wikitrust_revision where page_id = %s order by time_string desc limit 40",
                 (page_id, ))
    sl = curs.fetchall()
    for s in sl:
      rev_id = int(s[0])
      rev_date = s[1]
      revision_age = current_time - time_of_date(rev_date)
      if revision_age > 3 * 30 * 24 * 3600:
        return ret
      (d, quality, _) = classify(rev_id)
      if quality < 0.3:
        # This is likely vandalism.
        ret.append((title, rev_id, rev_date))
  return ret


def list_recent_revisions(page_id, n_revisions):
  """Returns a list of recent revisions for a page, up to a specified maximum."""
  curs.execute("select revision_id from " +
               ini_config.get('db', 'prefix') + 
               "wikitrust_revision where page_id = %s order by time_string desc limit %s",
               (page_id, n_revisions))
  return curs.fetchall()


def is_page_ok(page_id, min_revisions, min_length):
  """Checks whether a page has at least a given number of revisions and length."""
  # Just to be safe
  min_revisions = max(1, min_revisions)
  curs.execute("select count(*) from " +
               ini_config.get('db', 'prefix') +
               "wikitrust_revision where page_id = %s", (page_id,))
  n_revisions = int(row[0])
  if n_revisions < min_revisions:
    return false
  curs.execute("select rev_len from " +
               ini_config.get('db', 'prefix') + 
               "revision where rev_page = %s order by rev_timestamp desc limit %s",
               (page_id, min_revisions))
  len_list = cursor.fetchall()
  avg_length = sum([int(l[0]) for l in len_list]) / (1.0 * min_revisions)
  return avg_length >= min_length
  

def get_random_page_id(min_revisions=0, min_length=5):
  """Returns a random page_id from the Wikipedia."""
  while True do:
    r = random.random()
    curs.execute("select page_id from " +
                 ini_config.get('db', 'prefix') + 
                 "page where page_random > %s order by page_random asc",
                 (r, ))
    row = curs.fetchone()
    if len(row) == 0:
      continue
    page_id = int(row[0])
    # Eliminates pages with too few revisions or too short.
    if not is_page_ok(page_id, min_revisions, min_length):
      continue
    return page_id


def compute_quality_stats(page_id, num_revisions):
  """Computes quality statistics for the most recent num_revisions of
  a page."""
  curs.execute("select revision_id from " +
               ini_config.get('db', 'prefix') + 
               "wikitrust_revision where page_id = %s order by time_string desc limit %s",
               (page_id, num_revisions + 1))
  rev_list = [int(r[0]) for r in curs.fetchall()]
  n_vandalism = 0.0
  n_neg_qual = 0.0
  n_reverts = 0.0
  total_quality = 0.0
  total_text = 0.0
  total_untrusted_text = 0.0
  total_trust = 0.0
  total_reputation = 0.0
  n_revisions = 1.0 * len(rev_list - 1)
  # We start the analysis from 1, because we don't have information on
  # the very latest revision.
  for revision_id in rev_list[1:]:
    d = classify(revision_id)
    if d["Quality"] < 0.5:
      n_vandalism += 1.0
    if d["Avg_quality"] < 0.0:
      n_neg_quality += 1.0
    if d["Avg_quality"] < 0.7:
      n_reverts += 1.0
    total_quality += d["Avg_quality"]
    total_text += d["Curr_length"]
    for i in range(10):
      n = "Hist" + str (i)
      total_trust += i * d[n]
    for i in range(4):
      n = "Hist" + str (i)
      total_untrusted_text += d[n]
    total_reputation += math.log(1.0 + d["Reputation"])
  # Produces the output.
  out = {}
  out["Page_id"] = page_id
  out["Average_length"] = total_text / n_revisions
  out["Frac_vandalism"] = n_vandalism / n_revisions
  out["Frac_neg_qual"] = n_neg_qual / n_revisions
  out["Frac_reverts"] = n_reverts / n_revisions
  out["Avg_quality"] = total_quality / n_revisions
  out["Avg_untrusted_text"] = total_untrusted_text / n_revisions
  out["Frac_untrusted_text"] = total_untrusted_text / total_text
  out["Average_trust"] = total_trust / total_text
  out["Average_reputation"] = total_reputation / n_revisions
  return out
                                 
    

    
    
