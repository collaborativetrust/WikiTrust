<?php

# Copyright (c) 2007-09 The Regents of the University of California
# All rights reserved.
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

if (!defined('MEDIAWIKI')) die();

# There isn't a built in enum for php
global $wgWikiTrustVersion;
$wgWikiTrustVersion = "local"; ## This needs to be one of local, remote, wmf.

global $wgWikiTrustGadget, $wgWikiTrustShowVoteButton, $wgWikiTrustContentServerURL;
$wgWikiTrustGadget = NULL;
$wgWikiTrustShowVoteButton = true; // If true, the vote button is shown.
$wgWikiTrustContentServerURL = "http://localhost:10303/?";

// Debugging Verbosity
define(WIKITRUST_DEBUG, 0);
define(WIKITRUST_WARN, 10);
define(WIKITRUST_ERROR, 20);

global $wgWikiTrustLog, $wgWikiTrustDebugLog;
$wgWikiTrustLog = "/tmp/{$wgDBname}-trust.log";
$wgWikiTrustDebugLog = "/tmp/{$wgDBname}-trust-debug.log";
$wgWikiTrustDebugVerbosity = WIKITRUST_WARN; // how much information to write;
#$wgWikiTrustLog = "/dev/null";
#$wgWikiTrustDebugLog = "/dev/null";
global $wgWikiTrustCmd, $wgWikiTrustCmdExtraArgs, $wgWikiTrustColorPath,
	$wgWikiTrustRepSpeed, $wgWikiTrustApiURL;
$wgWikiTrustCmd = dirname(__FILE__) . "/eval_online_wiki";
$wgWikiTrustCmdExtraArgs = "";
$wgWikiTrustColorPath = NULL;
$wgWikiTrustRepSpeed = 1.0;
$wgWikiTrustApiURL = "http://en.wikipedia.org/w/api.php?";

global $wgExtensionFunctions, $wgExtensionCredits;
$wgExtensionFunctions[] = 'wfWikiTrustSetup';
$wgExtensionCredits['other'][] = array(
       'name' => 'WikiTrust',
       'author' => 'Ian Pye, Luca de Alfaro, Bo Adler',
       'url' => 'http://trust.cse.ucsc.edu',
       'description' => 'Adds trust tab to visualize article trust and provide text attribution.'
   );


// Quick debugging functions -- 
// They add a debugging level and call WikiTrust::Debug.
function wfWikiTrustDebug($msg){
  WikiTrust::debug($msg, WIKITRUST_DEBUG);
}

function wfWikiTrustWarn($msg){
  WikiTrust::debug($msg, WIKITRUST_WARN);
}

function wfWikiTrustError($msg){
  WikiTrust::debug($msg, WIKITRUST_ERROR);
}

function wfWikiTrustSetup() {
    $dir = dirname(__FILE__) . '/includes/';

    global $wgExtensionMessagesFiles;
    $wgExtensionMessagesFiles['WikiTrust'] = $dir.'/WikiTrust.i18n.php';

    global $wgAutoloadClasses, $wgHooks, $wgWikiTrustVersion;
    $wgAutoloadClasses['WikiTrustBase'] = $dir . 'WikiTrustBase.php';
    $wgAutoloadClasses['WikiTrustUpdate'] = $dir . 'WikiTrustUpdate.php';
    switch ($wgWikiTrustVersion) {
      case "local":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'LocalMode.php';
	$wgHooks['LoadExtensionsSchemaUpdates'][] = 'WikiTrustUpdate::updateDB';
	break;
      case "remote":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'RemoteMode.php';
	$wgHooks['LoadExtensionsSchemaUpdates'][] = 'WikiTrustUpdate::updateDB';
	// We always want to show colored output in RemoteMode
	$wgHooks['OutputPageBeforeHTML'][] = 'WikiTrust::ucscOutputBeforeHTML';
        break;
      case "wmf":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'WmfMode.php';
	break;
      default:
	die("Set \$wgWikiTrustVersion to one of 'local', 'remote', 'wmf' (not '$wgWikiTrustVersion')\n");
    }

    global $wgAjaxExportList, $wgUseAjax;
    if ($wgUseAjax) {
	$wgAjaxExportList[] = 'WikiTrust::handleVote';
	$wgAjaxExportList[] = 'WikiTrust::getColoredText';
    }

    # Is the user opting to use wikitrust?
    global $wgUser;
    if ($wgWikiTrustGadget && !$wgUser->getOption($wgWikiTrustGadget))
	return;

    # Add an extra tab
    $wgHooks['SkinTemplateTabs'][] = 'WikiTrust::ucscTrustTemplate';

    # Edit hook to notify
    # TODO: In 'remote' mode, we want to disable editing!
    $wgHooks['ArticleSaveComplete'][] = 'WikiTrust::ucscArticleSaveComplete';


    # We are done if the trust tab isn't selected
    global $wgRequest;
    $use_trust = $wgRequest->getVal('trust'); 
    if (!isset($use_trust) || 
        (($wgRequest->getVal('action') && 
          ($wgRequest->getVal('action') != 'purge'))))
	return;

    $wgHooks['OutputPageBeforeHTML'][] = 'WikiTrust::ucscOutputBeforeHTML';
    $wgHooks['OutputPageCheckLastModified'][] = 'WikiTrust::ucscOutputModified';
}

?>
