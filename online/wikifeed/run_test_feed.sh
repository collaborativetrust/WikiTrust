#!/bin/bash

BASE_DIR="/home/ipye/wikifeed"
NUM_REVS=$1
NUM_PULL=$2

## first setup the test
## $BASE_DIR/set_test.py
## then pull the revs
i=0
while [ $i -lt $NUM_REVS ]
do
  $BASE_DIR/pull_text.py 
  let i=$i+$NUM_PULL
done

## and the users
$BASE_DIR/pull_users.py 
