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
  
}    

?>
