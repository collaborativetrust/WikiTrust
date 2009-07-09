#!/usr/bin/python

# [mark_for_coloring (rev_id, page_id, user_id, rev_time, page_title, r_type)]
# Adds a revision into the db for coloring.
def mark_for_coloring (rev_id, page_id, user_id, rev_time, page_title, r_type,
                       connection, DB_PREFIX):

  curs = connection.cursor()
  sql = """SELECT revision_id FROM """ + DB_PREFIX + """wikitrust_missing_revs WHERE revision_id = %(rid)s AND processed <> 'processed'"""
  args = {'rid':rev_id}
  curs.execute(sql, args)
  numRows = curs.execute(sql, args)

  if (numRows <= 0):
     sql = """INSERT INTO """ + DB_PREFIX + """wikitrust_missing_revs (revision_id, page_id, page_title, rev_time, user_id, type) VALUES (%(rid)s, %(pid)s, %(title)s, %(time)s, %(vid)s, %(ty)s) ON DUPLICATE KEY UPDATE requested_on = now(), processed = 'unprocessed', type=%(ty)s"""
     args = {'rid':rev_id, 'pid':page_id, 'title':page_title, 
             'time':rev_time, 'vid':user_id, 'ty':r_type}
     curs.execute(sql, args)
     connection.commit()
