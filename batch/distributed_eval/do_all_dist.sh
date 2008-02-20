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


## top level command to to distributed coloring

DEFAULT_NUM=1
CLEANUP=$3
NUMBER=$2
RUN=$1
OUTPREFIX="/bigspace/cluster/results/"
SVNPRE="./bin/"
SHAREDPRE="/bigspace/cluster/"
#USER="trust"
HOST="gaston"
DESTINATION="/bigspace/cluster/results/"

CONST_USERS="panoramix:/big5/shared/rep-histories/enwiki-20070206-rel1.0-users"
# CONST_STATS="panoramix:/home/ipye/wiki/trunk/research/test_input/wiki1.stats"
 CONST_STATS="panoramix:/big5/shared/revquality.stats"
# CONST_STATS="panoramix:/big5/shared/samples_100/stats/allpick.stat"

EVALWIKI=$SVNPRE"evalwiki"
GRABER=$SVNPRE"grab_files_to_color" 

#STATS=$SHAREDPRE"stats/allpick.stat"
STATS=$SHAREDPRE"stats/revquality.stats"
USERS=$SHAREDPRE"users/enwiki-20070206-rel1.0-users"

#TRUST_KEY=".ssh/id_trust"

E_BADARGS=65

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` <experiment name>, (<temp dir number>)"
  exit $E_BADARGS
fi

## add the trust key to our keychain
#ssh-add $TRUST_KEY

if [ ! "$NUMBER" ]
then
  NUMBER=$DEFAULT_NUM
fi

## try to get the files we need over here
if [ ! -f $STATS ]
then
  scp $USER"@"$CONST_STATS $STATS
fi

if [ ! -f $USERS ]
then
  scp $USER"@"$CONST_USERS $USERS
fi

## do we have a dir to run in?
if [ ! -d $OUTPREFIX ]
then
  mkdir $OUTPREFIX 
fi

#-stat_file \                                                                            
 # $STATS  \ 

echo "running eval"

 $EVALWIKI  \
 -do_dist -dist_client  \
 $GRABER \
 -remote_host $HOST -remote_user $USER \
 -remote_color_dir $DESTINATION \
 -begin_url \
 http://trust:slugsrule@panoramix.cse.ucsc.edu:801/get_file_by_expr.py?e=$RUN\\\&n=1 \
 -done_url \
 http://trust:slugsrule@panoramix.cse.ucsc.edu:801/mark_file_processed.py?e=$RUN\\\&file= \
 -src_dir $OUTPREFIX"src" \
 -d $OUTPREFIX"colored" -historyfile \
 "$USERS"  \
 -loop_until_done \
 -eval_local_trust \
 -rep_lends_trust 0.4 -trust_read_all 0.2 -trust_read_part 0.2 \
 -trust_radius 2.0 -trust_part_radius 4.0 \
 -bad_qual_thrs -0.7 \
 -trace_words -gen_color -gen_eval -do_origin  

#if [ "$CLEANUP" ]
#then
 # rm $USERS
 # rm $STATS
 # rm -r $OUTPREFIX
#fi

