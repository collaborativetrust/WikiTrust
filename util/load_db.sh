#!/bin/bash

base_dir=$1
logfile=$2
#db_user=$2
#db_name=$3
#db_pass=$4

if [ -z $logfile ]; then
  echo "usage: ./load_db.sh (base_dir or sql_file) logfile | mysql -u dbuser dbname -p"
  exit 0
fi

start_tx="BEGIN;"
drop_page_idx="DROP INDEX wikitrust_page_title_idx ON wikitrust_page;"
drop_rev_idx="DROP INDEX wikitrust_revision_id_timestamp_idx ON wikitrust_revision;"
create_page_idx="CREATE INDEX wikitrust_page_title_idx ON wikitrust_page (page_title);"
create_rev_idx="CREATE INDEX wikitrust_revision_id_timestamp_idx ON wikitrust_revision (page_id, time_string, revision_id);"
end_tx="COMMIT;"

# drop the indicis
echo $drop_page_idx
echo $drop_rev_idx

# how are we doing?
cma=0
files_seen=0

if [ -f "$base_dir" ]; then
  cat $base_dir
else
  for file in `find $base_dir/sql -name *.sql | sort -id`; do
    stime=`date +%s`
    echo $start_tx
    size=$(stat -c%s "$file")
    cat $file
    echo $end_tx
    etime=`date +%s`
    let elapsed=$etime-$stime
    if [ "$elapsed" -eq "0" ]; then
      let elapsed=1
    fi
    let bytes_sec=$size/$elapsed
    let cma=$files_seen*$cma
    let cma=$bytes_sec+$cma
    let files_seen=$files_seen+1
    let cma=$cma/$files_seen
    echo "bytes/sec: $bytes_sec (spot) $cma (cma) $file" >> $logfile
  done
  if [ -f "$base_dir/user_reputations.txt" ]; then
    echo "REMEMBER: cat $base_dir/user_reputations.txt | ../analysis/load_reputations -db_user " >> $logfile
  fi
fi

# and re-create them
echo $create_page_idx
echo $create_rev_idx

