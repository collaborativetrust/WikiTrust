<?php

# Copyright (c) 2009 B. Thomas Adler
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

if (!defined('MEDIAWIKI')) die();

# There isn't a built in enum for php
global $wgWikiTrustVersion;
$wgWikiTrustVersion = "local"; ## This needs to be one of local, remote, wmf.

global $wgWikiTrustGadget, $wgWikiTrustShowVoteButton, $wgWikiTrustContentServerURL;
$wgWikiTrustGadget = "gadget-WikiTrust";
$wgWikiTrustShowVoteButton = true; // If true, the vote button is shown.
$wgWikiTrustContentServerURL = "http://localhost:10303/?";

global $wgWikiTrustLog, $wgWikiTrustDebugLog;
$wgWikiTrustLog = "/tmp/{$wgDBname}-trust.log";
$wgWikiTrustDebugLog = "/tmp/{$wgDBname}-trust-debug.log";
global $wgWikiTrustCmd, $wgWikiTrustRepSpeed, $wgWikiTrustApiURL;
$wgWikiTrustCmd = $IP . "/eval_online_wiki";
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


function wfWikiTrustSetup {
    $dir = dirname(__FILE__) . '/';

    // TODO: Does this actually do anything??  It doesn't match
	// code in other projects.
    // ParserFirstCallInit was introduced in modern (1.12+) MW versions
    // so as to avoid unstubbing $wgParser on setHook() too early, as
    // per r35980.
    if (!defined( 'MW_SUPPORTS_PARSERFIRSTCALLINIT' )) {
      global $wgParser;
      wfRunHooks( 'ParserFirstCallInit', $wgParser );
    }


    global $wgExtensionMessagesFiles;
    $wgExtensionMessagesFiles['WikiTrust'] = $dir.'/WikiTrust.i18n.php';

    global $wgAutoloadClasses;
    $wgAutoloadClasses['WikiTrustUpdate'] = $dir . 'WikiTrustUpdate.php';
    $wgAutoloadClasses['WikiTrustBase'] = $dir . 'WikiTrustBase.php';
    switch ($wgWikiTrustVersion) {
      case "local":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'LocalMode.php';
	break;
      case "remote":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'RemoteMode.php';
        break;
      case "wmf":
	$wgAutoloadClasses['WikiTrust'] = $dir . 'WmfMode.php';
	break;
      default:
	die("Set \$wgWikiTrustVersion to one of 'local', 'remote', 'wmf'\n");
    }

    # Updater fired when updating to a new version of MW.
    global $wgHooks;
    $wgHooks['LoadExtensionsSchemaUpdates'][] = 'WikiTrust::updateDB';

    # Is the user opting to use wikitrust
    global $wgUser;
    if ($wgWikiTrustGadget && !$wgUser->getOption($wgWikiTrustGadget)) {
	return;
    }

    # Add an extra tab
    $wgHooks['SkinTemplateTabs'][] = 'WikiTrust::ucscTrustTemplate';

    # Edit hook to notify
    $wgHooks['ArticleSaveComplete'][] = 'WikiTrust::ucscArticleSaveComplete';

    global $wgAjaxExportList, $wgUseAjax;
    if ($wgUseAjax) {
	if ($wgWikiTrustShowVoteButton) {
	    $wgAjaxExportList[] = 'WikiTrust::handleVote';
	}
	$wgAjaxExportList[] = 'WikiTrust::getColoredText';
    }

    # That's it if the trust tab isn't selected
    global $wgRequest;
    if (!$wgRequest->getVal('trust') || $wgRequest->getVal('action')) {
	return;
    }

    $wgHooks['OutputPageBeforeHTML'][] = 'WikiTrust::ucscOutputBeforeHTML';
}

?>
