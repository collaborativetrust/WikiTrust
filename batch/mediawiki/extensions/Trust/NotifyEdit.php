<?php

# Copyright (c) 2007 Ian Pye
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA


/**
  MW Extension

  This extension notifies a service that a aritcle revision has been sucessfully saved

  Ian Pye <ipye@cs.ucsc.edu>

*/

$F_ADDRESS = "panoramix.cse.ucsc.edu";
$F_PORT = "10123";
$SEK = "sjka48hfkds75QQ:";

$DEBUG = false;
$LOG_FILE = "/tmp/notify_exit.log";

## add this to the hooks array
$wgHooks['ArticleSaveComplete'][] = 'notifyArticleSave';

function notifyArticleSave(&$article, &$user, &$text, &$summary, &$minoredit, &$watchthis, &$sectionanchor, &$flags, &$revision) { 
 
  global $F_ADDRESS, $F_PORT, $DEBUG, $LOG_FILE, $SEK;

  if(!$revision){
    return false;
  }

  $msg = $SEK . $revision->getId();
  $errno;
  $errstr;

  $sock=fsockopen( "udp://" . $F_ADDRESS, $F_PORT, $errno, $errstr);
  if($sock){
    $ret=fwrite($sock,$msg);
    fclose($sock);

    if($DEBUG){
      file_put_contents ( $LOG_FILE, date("c") . " : " . "$ret : success\n"
        , FILE_APPEND );
    }
  } else if($DEBUG) {
    file_put_contents ( $LOG_FILE, date("c") . " : " . "$msg : error - $errno 
        : $errstr\n", FILE_APPEND);
  }

  if($ret)
    return(true);
  
  return(false);  
}


?>
