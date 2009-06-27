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

## MW extension
# This defines a custom MW function to map trust values to HTML markup
# 
# Uses Tool Tip JS library under the LGPL.
# http://www.walterzorn.com/tooltip/tooltip_e.htm

class WikiTrust {
  
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
    // TODO(Bo): is this right?  Run ParserFirstCallInit if it's not defined?
    // This doesn't match code from all the other examples I could find.
    if (!defined( 'MW_SUPPORTS_PARSERFIRSTCALLINIT' )) {
      global $wgParser;
      // TODO(Bo): this is already declared global above...
      wfRunHooks( 'ParserFirstCallInit', $wgParser );
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
  // TODO(Bo): What does this mean?  Many of the other functions
  // just call the Impl class.  Is there a speed optimization here?
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
