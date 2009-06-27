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


class TextTrust {

  /**
   * Initializes and configures the extension.
   */
  public static function init() {
    global $wgHooks, $wgParser, $wgRequest, $wgUseAjax, $wgAjaxExportList, 
      $wgUser, $wgOut, $wgScriptPath, $wgExtensionMessagesFiles, 
      $wgWikiTrustShowVoteButton;
    
    
    $wgHooks['OutputPageBeforeHTML'][] ='TextTrust::ucscColorTrust_OP';
  }

  public static function ucscColorTrust_OP(&$out, &$text){
    global $wgScriptPath;
    wfLoadExtensionMessages('RemoteTrust');

    $out->addScript("<script type=\"text/javascript\" src=\"".$wgScriptPath."/extensions/WikiTrust/js/trust.js\"></script>\n");
    $out->addScript("<link rel=\"stylesheet\" type=\"text/css\" href=\"".$wgScriptPath."/extensions/WikiTrust/css/trust.css\" />\n");

    $ctext_html = "<div id='text-button'><input type='button' name='ctext' value='getColoredText' onclick='startGetColoredText()'></div>";
    $vtext_html = "<div id='vote-button'><input type='button' name='vote' value='" . wfMsgNoTrans("wgVoteText") . "' onclick='startVote()' /></div><div id='vote-button-done'>". wfMsgNoTrans("wgThankYouForVoting") ."</div>";
    
    $out->addHTML($ctext_html);
    $out->addHTML($vtext_html);

    return true;
  }
}    

?>
