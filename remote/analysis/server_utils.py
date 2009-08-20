#!/usr/bin/python

# [mark_for_coloring (rev_id, page_id, user_id, rev_time, page_title, r_type)]
# Adds a revision into the db for coloring.
def mark_for_coloring (rev_id, page_id, user_id, rev_time, page_title, r_type,
                       connection, DB_PREFIX):

  curs = connection.cursor()
  sql = """SELECT page_title FROM """ + DB_PREFIX + """wikitrust_queue WHERE page_title = %(pid)s AND processed <> 'processed'"""
  args = {'pid':page_title}
  curs.execute(sql, args)
  numRows = curs.execute(sql, args)

  if (numRows <= 0):
     sql = """INSERT INTO """ + DB_PREFIX + """wikitrust_queue (page_id, page_title) VALUES (%(pid)s, %(title)s) ON DUPLICATE KEY UPDATE requested_on = now(), processed = 'unprocessed'"""
     args = {'pid':page_id, 'title':page_title}
     curs.execute(sql, args)
     connection.commit()
