<?php

# Copyright (c) 2007,2008 Luca de Alfaro
# Copyright (c) 2007,2008 Ian Pye
# Copyright (c) 2007 Jason Benterou
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

# There isn't a built in enum for php
$wgWikiTrustVersion = "mwf"; ## This needs to be one of local, remote, wmf.

$wgWikiTrustGadget = "gadget-WikiTrust";
$wgWikiTrustShowVoteButton = true; // If true, the vote button is shown.
$wgWikiTrustContentServerURL = "http://localhost:10303/?";

$wgTrustCmd = $IP . "/eval_online_wiki";
$wgTrustLog = "/tmp/{$wgDBname}-trust.log";
$wgTrustDebugLog = "/tmp/{$wgDBname}-trust-debug.log";
$wgRepSpeed = 1.0;
$wgWikiApiURL = "http://en.wikipedia.org/w/api.php?";

switch ($wgWikiTrustVersion) {
  case "local":
	  require_once("$IP/extensions/WikiTrust/includes/LocalTrust.php");
	  break;
  case "remote":
	  require_once("$IP/extensions/WikiTrust/includes/RemoteTrustAjax.php");
	  break;
  case "mwf":
	  require_once("$IP/extensions/WikiTrust/includes/RemoteTrust.php");
	  break;
  default:
    die("Set \$wgWikiTrustVersion to one of 'local', 'remote', 'mwf'\n");
}

?>
