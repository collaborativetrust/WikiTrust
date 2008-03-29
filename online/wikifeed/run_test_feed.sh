#!/bin/bash

BASE_DIR="/home/ipye/wikifeed"

## first setup the test
## $BASE_DIR/set_test.py
## then pull the revs
for j in 0 1 2 3; do for i in 0 1 2 3 4 5 6 7 8 9; do $BASE_DIR/pull_text.py; done ;done

## and the users
$BASE_DIR/pull_users.py 
