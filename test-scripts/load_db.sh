#!/bin/bash

sql_dir=$1
db_user=$2
db_name=$3
db_pass=$4

if [ -z $db_pass ]; then
  echo "usage: ./load_db.sh sql_dir dbuser dbname dbpass"
  exit 0
fi

drop_page_idx="DROP INDEX wikitrust_page_title_idx ON wikitrust_page"
drop_rev_idx="DROP INDEX wikitrust_revision_id_timestamp_idx ON wikitrust_revision"
create_page_idx="CREATE INDEX wikitrust_page_title_idx ON wikitrust_page (page_title)"
create_rev_idx="CREATE INDEX wikitrust_revision_id_timestamp_idx ON wikitrust_revision (page_id, time_string, revision_id)"

# drop the indicis
`echo $drop_page_idx | mysql -u $db_user -p$db_pass $db_name`
`echo $drop_rev_idx | mysql -u $db_user -p$db_pass $db_name`

for file in `find $sql_dir -name *.sql`; do
  `cat $file | mysql -u $db_user -p$db_pass $db_name`
done

# and re-create them
`echo $create_page_idx | mysql -u $db_user -p$db_pass $db_name`
`echo $create_rev_idx | mysql -u $db_user -p$db_pass $db_name`

