#!/bin/bash

# Copyright (c) 2007-2008 The Regents of the University of California
# All rights reserved.

# Authors: Ian Pye

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.

# 3. The names of the contributors may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Grabs a set of files from the fileserver to color
## and sends them back to where they came from
## Usage: ./grab_files_to_color experiment
## Ian Pye <ipye@cs.ucsc.edu>

## now expects a nfs mount to write files to/from

## Possible error end states
E_BADARGS=65
TRAP=-1

## Arguments
HOST_URL="$1"
DONE_URL="$2"
RHOST="$3"
R_COLOR_DIR="$4"
RUSER="$5"
LOCAL_SRC_DIR="$6"
LOCAL_COLOR_DIR="$7"
DO_END="$8"

## Control Chars
DO_END_CHAR="--end"
END_CHAR="0000"
START_STATE="1111"
END_STATE="2222"
CURRENT_STATE=$START_STATE

## keep looping grabbing files untill the total size is more than this
MIN_SIZE_TO_RUN=100
MAX_LOOP=10

## Comands
WGET="curl -s"
SCP="scp"
CP="cp"

trap "exit $TRAP" SIGINT SIGTERM

if [ ! -n "$7" ]
then
  echo $1
  echo "Usage: `basename $0` <full url of file server> <mark as finished url> <rhost> <r_color_dir> <ruser> <state>"
  echo "example: ./color_all_dist http://trust:slugsrule@panoramix.cse.ucsc.edu:801/get_file_by_expr.py?e=one&n=3 /"
  echo "http://trust:slugsrule@panoramix.cse.ucsc.edu:801/mark_file_processed.py?e=one&file="
  exit $E_BADARGS
fi

## check the current state
if [ "$DO_END_CHAR" = "$DO_END" ]
then
  CURRENT_STATE=$END_STATE
fi

## make sure the temp dirs are there when we need them
if [ ! -d "$LOCAL_SRC_DIR" ]
then        
  mkdir $LOCAL_SRC_DIR
fi

if [ ! -d "$LOCAL_COLOR_DIR" ]
then
  mkdir $LOCAL_COLOR_DIR
fi

if [ "$CURRENT_STATE" = "$START_STATE" ]
then
  files=`$WGET $HOST_URL` 

  local_files=""
  src_size=`ls -l "$LOCAL_SRC_DIR/" | awk 'BEGIN {sum=0} {sum += $5}END {print sum}'`

  ## loop untill all of the files we asked for are downloaded
  for file in $(echo "$files" |  sed 's/,/ /g' )
  do
    echo "$file"
    local_files="$local_files $file"

    ## echo "fetching $file"
    #res=`$SCP $RUSER"@"$RHOST":"$file $LOCAL_SRC_DIR`
   # res=`$CP $file $LOCAL_SRC_DIR/`
  done

  #src_size=`ls -l "$LOCAL_SRC_DIR/" | awk 'BEGIN {sum=0} {sum += $5}END {print sum}'`

 # for file in `ls "$LOCAL_SRC_DIR/"`                                                         
 # do
 #   echo "$LOCAL_SRC_DIR/$file"
 #   local_files="$local_files $LOCAL_SRC_DIR/$file"         
 # done  
  echo $END_CHAR
## START END STATE
elif [ "$CURRENT_STATE" = "$END_STATE" ]
then
  echo "Entering End State"
  #for file in `ls "$LOCAL_COLOR_DIR/"`
  #do
   # scp_status=-1
   # count=0
  #  until [ $scp_status -eq 0 -o $count -gt $MAX_LOOP ]; do 
   #   echo "sending $file back"                                                                 
   #   result=`$WGET $DONE_URL$file`                                                          
   #   echo $result   
      #res=`$SCP $LOCAL_COLOR_DIR/$file $RUSER"@"$RHOST":"$R_COLOR_DIR/`
  #    res=`$CP $LOCAL_COLOR_DIR/$file $R_COLOR_DIR/`
  #    scp_status=$?
  #    count=$(($count+1))
  #    sleep 1
  #  done 
    
   # if [ $scp_status -eq 0 ]
   # then
   #   echo "now deleteing $file"
   #   rm $LOCAL_COLOR_DIR/$file
   # else
   #   echo "giving up on sending back $file, for now"
   # fi
 # done        

 #for file in `ls "$LOCAL_SRC_DIR/"`
 #do
 #  echo "marking $file as processed"
 #  result=`$WGET $DONE_URL$file`
 #  echo $result
 #done  

  ## cleanup
  ##`rm $LOCAL_SRC_DIR/*`
  
  ## and that's it
  echo $END_CHAR
fi ## END END_STATE  
exit 0;
