#!/bin/bash

PROC_LIM=65
START=172

WORKING_DIR=$1
RESULTS_DIR=$2
PROGRAM=$3

SERVERS=( issdm-20 issdm-21 issdm-22 issdm-23 issdm-24 issdm-25 issdm-26 issdm-27 issdm-28 issdm-29 issdm-30 issdm-31 issdm-32 issdm-33 issdm-34 issdm-35 issdm-36 issdm-37 issdm-38 issdm-39 issdm-40 issdm-41 issdm-42 issdm-43 issdm-44 issdm-45 issdm-46 issdm-47 )

count=0
next_server=0
for file in `ls $WORKING_DIR`; do
  if [ "$count" -ge "$START" ]; then
    echo ${SERVERS[$next_server]} $file
    ssh ${SERVERS[$next_server]} "$PROGRAM -d $RESULTS_DIR -compute_stats $WORKING_DIR/$file" &
    #echo ssh ${SERVERS[$next_server]} "$PROGRAM -d $RESULTS_DIR -compute_stats $WORKING_DIR/$file" &
    let next_server=next_server+1
    if [ "$next_server" -ge "${#SERVERS[@]}" ]; then
      let next_server=0
    fi
    sleep 1
  fi
  let count=count+1
  current_proc=`ps -A -f | grep evalwiki | wc -l`
  until [ "$current_proc" -le "$PROC_LIM" ]; do
    sleep 1
    let current_proc=`ps -A -f | grep evalwiki | wc -l`
  done
done
