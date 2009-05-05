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

## MW extension
# This defines a custom MW function to map trust values to HTML markup
# 
# Uses Tool Tip JS library under the LGPL.
# http://www.walterzorn.com/tooltip/tooltip_e.htm

$dir = dirname(__FILE__) . '/'; 

$wgExtensionMessagesFiles['RemoteTrust'] = $dir.'/RemoteTrust.i18n.php';

# Credits
$wgExtensionCredits['other'][] = array(
				       'name' => 
				       'Text Attribution and Trust',
				       'author' =>
				       'Ian Pye, Luca de Alfaro', 
				       'url' => 
				       'http://trust.cse.ucsc.edu', 
				       'description' => 
				       'Text is colored according to how much it has been revised.  An orange background indicates new, unrevised, text;  white is for text that has been revised by many reputed authors.  If you click on a word, you will be redirected to the diff corresponding to the edit where the word was introduced.  If you hover over a word, a pop-up displays the word author.'
				       );
			 
$wgAutoloadClasses['TextTrustImpl'] = $dir . 'LocalTrustImpl.php';
$wgAutoloadClasses['TextTrustUpdate'] = $dir . 'RemoteTrustUpdate.php';
$wgExtensionFunctions[] = 'TextTrust::init';

// Vote handleing
$wgAjaxExportList[] = "TextTrustImpl::handleVote";

class TextTrust {
  
  // Instance of our TrustImpl class
  private static $trust;

  /**
   * Initializes and configures the extension.
   */
  public static function init() {
    global $wgHooks, $wgParser, $wgRequest, $wgUseAjax, $wgAjaxExportList, 
      $wgUser, $wgOut, $wgScriptPath, $wgExtensionMessagesFiles, 
      $wgWikiTrustShowVoteButton, $wgWikiTrustGadget;
    
    // ParserFirstCallInit was introduced in modern (1.12+) MW versions 
		// so as to avoid unstubbing $wgParser on setHook() too early, as 
		// per r35980.
    if (!defined( 'MW_SUPPORTS_PARSERFIRSTCALLINIT' )) {
      global $wgParser;
      wfRunHooks( 'ParserFirstCallInit', $wgParser );
    }

# Updater fired when updating to a new version of MW.
    $wgHooks['LoadExtensionSchemaUpdates'][] = 'TextTrustUpdate::updateDB';
      
# Is the user opting to use wikitrust?
    if ($wgWikiTrustGadget && !$wgUser->getOption( $wgWikiTrustGadget)){
			return;
    }
 
# And add an extra tab.
    $wgHooks['SkinTemplateTabs'][] = 'TextTrust::ucscTrustTemplate';
  
# Edit hook.
		$wgHooks['ArticleSaveComplete'][] = 'TextTrust::ucscArticleSaveComplete';

# Return if trust is not selected.
    if(!$wgRequest->getVal('trust') || $wgRequest->getVal('action')){
      return;
    }
    
# Add colored text if availible.
    $wgHooks['OutputPageBeforeHTML'][] = 'TextTrust::ucscOutputBeforeHTML';	
	}

  private static function getTrustObj(){
    if (!self::$trust){
      self::$trust = new TextTrustImpl();
    }

		return;
  }

  // Handle trust tab.
  // This is here because we use it all the time. 
  public static function ucscTrustTemplate($skin, &$content_actions){
    global $wgRequest;
    wfLoadExtensionMessages('RemoteTrust');
    
    if ($wgRequest->getVal('action') || $wgRequest->getVal('diff')){
      // we don't want trust for actions or diffs.
      return true;
    }
    
    $trust_qs = $_SERVER['QUERY_STRING'];
    if($trust_qs){
      $trust_qs = "?" . $trust_qs .  "&trust=t";
    } else {
      $trust_qs .= "?trust=t"; 
    }
    
    $content_actions['trust'] = array ( 'class' => '',
					'text' => 
					wfMsgNoTrans("wgTrustTabText"),
					'href' => 
					$_SERVER['PHP_SELF'] . $trust_qs );
    
    if($wgRequest->getVal('trust')){
      $content_actions['trust']['class'] = 'selected';
      $content_actions['nstab-main']['class'] = '';
      $content_actions['nstab-main']['href'] .= '';
    } else {
      $content_actions['trust']['href'] .= '';
    }
    return true;
  }
  
  public static function ucscOutputBeforeHTML(&$out, &$text){
    self::getTrustObj();
    return self::$trust->ucscOutputBeforeHTML($out, $text);
  }

	public static function ucscArticleSaveComplete(&$article, 
																								 &$user, 
																								 &$text, 
																								 &$summary,
																								 &$minoredit, 
																								 &$watchthis, 
																								 &$sectionanchor, 
																								 &$flags, 
																								 &$revision){
		self::getTrustObj();
    return self::$trust->ucscArticleSaveComplete($article, 
																								 $user, 
																								 $text, 
																								 $summary,
																								 $minoredit, 
																								 $watchthis, 
																								 $sectionanchor, 
																								 $flags, 
																								 $revision);
	}
}    

?>
