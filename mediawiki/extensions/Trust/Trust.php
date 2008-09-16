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

class TextTrust extends ExtensionClass
{

  ## the css tag to use
  const TRUST_CSS_TAG = "background-color"; ## color the background
  #$TRUST_CSS_TAG = "color"; ## color just the text
  
  ## ColorText is called multiple times, but we only want to color true text.
  const DIFF_TOKEN_TO_COLOR = "Lin";

  ## Trust normalization values;
  const MAX_TRUST_VALUE = 9;
  const MIN_TRUST_VALUE = 0;
  const TRUST_MULTIPLIER = 10;
  
  ## Token to split trust and origin values on
  const TRUST_SPLIT_TOKEN = ',';

  ## Token to be replaed with <
  const TRUST_OPEN_TOKEN = "QQampo:";
  
  ## Token to be replaed with >
  const TRUST_CLOSE_TOKEN = ":ampc:";

  ## Median Value of Trust
  var $median = 0.0;

  ## Number of times a revision is looked at.
  var $times_rev_loaded = 0;

  ## Load the article we are talking about
  var $title;

  ## Only color the text once.
  var $colored = false;

  ## Don't close the first opening span tag
  var $first_span = true;

  ## And the same for origin tags
  var $first_origin = true;

  ## And the last revision of the title
  var $current_rev;

  ## Only add the scripts once.
  var $scripts_added = false;

  ## Should we do all the fancy trust processing?
  var $trust_engaged = false;
    
  ## map trust values to html color codes
  var $COLORS = array(
		  "trust0",
		  "trust1",
		  "trust2",
		  "trust3",
		  "trust4",
		  "trust5",
		  "trust6",
		  "trust7",
		  "trust9",
		  "trust10",
		  );

  ## Only write a new trust tag when the trust changes.
  var $current_trust = "trust0";
  
  var $trustJS = '
<script type="text/javascript">/*<![CDATA[*/
var ctrlState = false;
function showOrigin(revnum) {
  //if (ctrlState) {
    document.location.href = wgScriptPath + "/index.php?title=" + wgPageName + "&diff=" + revnum;
 // }
}

function checkForCtrlDown(event) {
  if (event.ctrlKey) { ctrlState = true; }
  //alert (event.keyCode + " " + event.ctrlKey + " " + ctrlState);
}

function clearCtrlState(event) {
  //alert(event.keyCode + " " + ctrlState);
  ctrlState = false;
}

function checkCtrlState(event) {
  //alert(ctrlState);
}

// The ILikeThis functionality
function iLikeThisCallback(http_request){
  if ((http_request.readyState == 4) && (http_request.status == 200)) {
    document.getElementById("agree-button-done").style.visibility = "visible";
    document.getElementById("agree-button").style.visibility = "hidden";
   // alert(http_request.responseText);
    return true;
  } else {
    alert(http_request.responseText);
    return false;
  }
}

function getQueryVariable(variable) {
  var query = window.location.search.substring(1);
  var vars = query.split("&");
  for (var i=0;i<vars.length;i++) {
    var pair = vars[i].split("=");
    if (pair[0] == variable) {
      return pair[1];
    }
  } 
  return "";
}

function startILikeThis(){

  var revID = getQueryVariable("oldid");
  if (revID == ""){
    revID = getQueryVariable("diff");
    if (revID == ""){
      revID = wgCurRevisionId;
    }
  }

  return sajax_do_call( "TextTrust::handleILikeThis", [wgUserName, wgArticleId, revID] , iLikeThisCallback ); 
}

/*]]>*/</script>';

  var $trustCSS = '
<style type="text/css">/*<![CDATA[*/
.trust0 {
  background-color: #FFB947;
}

.trust1 {
  background-color: #FFC05C;
}

.trust2 {
  background-color: #FFC870;
}

.trust3 {
  background-color: #FFD085;
}

.trust4 {
  background-color: #FFD899;
}

.trust5 {
  background-color: #FFE0AD;
}

.trust6 {
  background-color: #FFE8C2;
}

.trust7 {
  background-color: #FFEFD6;
}

.trust8 {
  background-color: #FFF7EB;
}

.trust9 {
  background-color: #FFFFFF;
}

.trust10 {
  background-color: #FFFFFF;
}

#agree-button-done {
  visibility: hidden;
  position: absolute;
  top: 10px;
  left: 500px;
}

#agree-button {
  position: absolute;
  top: 10px;
  left: 500px;
}

/*]]>*/</style>';

  public static function &singleton( )
  { return parent::singleton( ); }
  
  public function TextTrust() 
  {
   parent::__construct( );
   global $wgExtensionCredits;
   
# Define a setup function
   $wgExtensionFunctions[] = 'ucscColorTrust_Setup';

# Credits
   $wgExtensionCredits['parserhook'][] = array(
					       'name' => 'Trust Coloring',
					       'author' =>'Ian Pye', 
					       'url' => 
					       'http://trust.cse.ucsc.edu', 
					       'description' => 'This Extension 
colors text according to trust.'
					       );
  }
  
  public function setup()
  {
    parent::setup();
    global $wgHooks, $wgParser, $wgRequest, $wgUseAjax, $wgShowILike, $wgAjaxExportList;
   
# Code which takes the "I agree with this action".
# This has to be statically called.
    if($wgUseAjax && $wgShowILike){
      $wgAjaxExportList[] = "TextTrust::handleILikeThis";
    }
    
# And add and extra tab.
    $wgHooks['SkinTemplateTabs'][] = array( &$this, 'ucscTrustTemplate');

# And add a hook so the colored text is found. 
    $wgHooks['ParserBeforeStrip'][] = array( &$this, 'ucscSeeIfColored');

# Color saved text
    $wgHooks['ArticleSaveComplete'][] = array( &$this, 'ucscRunColoring');

# If the trust tab is not selected, or some other tabs are don't worry about things any more.
    if(!$wgRequest->getVal('trust') || $wgRequest->getVal('action')){
      $this->trust_engaged = false;
      return;
    } 
    $this->trust_engaged = true;
   
# Add trust CSS and JS
    $wgHooks['OutputPageBeforeHTML'][] = array( &$this, 'ucscColorTrust_OP');
 
# Add a hook to initialise the magic words
    $wgHooks['LanguageGetMagic'][] = array( &$this, 'ucscColorTrust_Magic');
   
# Set a function hook associating the blame and trust words with a callback function
    $wgParser->setFunctionHook( 't', array( &$this, 'ucscColorTrust_Render'));

# After everything, make the blame info work
    $wgHooks['ParserAfterTidy'][] = array( &$this, 'ucscOrigin_Finalize');
    
# Pull the median value
    $this->update_median();
  }
  
  /**
   
   */
  static function handleILikeThis($userName, $page_id = 0, $rev_id = 0){
    
    global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgTrustCmd, $wgTrustLog, $wgTrustDebugLog, $wgVoteRev;
   
    $response = new AjaxResponse("0");
    $command = "";
    $pid = -1;

    if($page_id){
      // First, look up the id numbers from the page and user strings
      $dbr =& wfGetDB( DB_SLAVE );
      $res = $dbr->select('user', array('user_id'), array('user_name' => $userName), array());
      if ($res){
	$row = $dbr->fetchRow($res);
	$user_id = $row['user_id'];
	if (!$user_id) {
	  $user_id = 0;
	}
      }
      $dbr->freeResult( $res );      

      // Then stick the stuff in.
      $command = "nohup $wgVoteRev -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname -voter_id $user_id -page_id $page_id -rev_id $rev_id >> $wgTrustDebugLog 2>&1 & echo $!";
    
      // Do something here to update the trust of this revision.
      $pid = shell_exec($command);
    }
    
    if($pid)
      $response = new AjaxResponse("$command");

    return $response;
  }

  /**
   Called just before rendering HTML.
   We add the coloring scripts here.
  */
  function ucscColorTrust_OP(&$out, &$text){
    if (!$this->scripts_added){ // Only add the scripts once.
      $out->addScript($this->trustJS); 
      $out->addScript($this->trustCSS);
      $this->scripts_added = true;
    }
    return true;
  }

 /**
  Updated the cached median reputation value.
 */
 function update_median(){
   $dbr =& wfGetDB( DB_SLAVE );
   $res = $dbr->select('wikitrust_global', 'median', array(), array());
   if ($res){
     $row = $dbr->fetchRow($res);
     $this->median = $row['median']; 
   } 
   $dbr->freeResult( $res );

   // check for divide by 0 errors.
   if ($this->median == 0)
     $this->median = 1;
   
   return $this->median;
 }
 
/* 
 Code to fork and exec a new process to color any new revisions.
 Called after any edits are made.
*/
 function ucscRunColoring(&$article, &$user, &$text, &$summary, $minor, $watch, $sectionanchor, &$flags, $revision) { 
   global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgTrustCmd, $wgTrustLog, $wgTrustDebugLog, $wgRepSpeed;
   
   $pid = -1;
   
   // Start the coloring.
   $command = "nohup $wgTrustCmd -rep_speed $wgRepSpeed -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname >> $wgTrustDebugLog 2>&1 & echo $!";
   // $pid = shell_exec("/bin/echo '$command' >> $wgTrustDebugLog");
   $pid = shell_exec($command);
  
   if($pid)
     return true;
   return false;
 }

# Actually add the tab.
 function ucscTrustTemplate($skin, &$content_actions) { 
  
   global $wgTrustTabText, $wgRequest;
   if (!isset($wgTrustTabText)){
     $wgTrustTabText = "trust";
   }
   
   if ($wgRequest->getVal('action')){
     // we don't want trust for actions.
     return true;
   }
   
   $trust_qs = $_SERVER['QUERY_STRING'];
   if($trust_qs){
     $trust_qs = "?" . $trust_qs .  "&trust=t";
   } else {
     $trust_qs .= "?trust=t"; 
   }
   
   $content_actions['trust'] = array ( 'class' => '',
				       'text' => $wgTrustTabText,
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
 
 /**
  If colored text exists, use it instead of the normal text, 
  but only if the trust tab is selected.
  
  TODO: Make this function work with caching turned on.
 */
 function ucscSeeIfColored(&$parser, &$text, &$strip_state) { 
   global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgTrustCmd, $wgTrustLog, $wgTrustDebugLog, $wgRepSpeed, $wgRequest, $wgTrustExplanation, $wgUseAjax, $wgShowILike;

   // Turn off caching for this instanching for this instance.
   $parser->disableCache();
   
   // Text for showing the "I like it" button
   $iLikeItText = "";
   if ($wgUseAjax && $wgShowILike){
     $iLikeItText = "
".self::TRUST_OPEN_TOKEN."div id='agree-button'".self::TRUST_CLOSE_TOKEN."".self::TRUST_OPEN_TOKEN."input type='button' name='agree' value='I agree with this text' onclick='startILikeThis()' /".self::TRUST_CLOSE_TOKEN."".self::TRUST_OPEN_TOKEN."/div".self::TRUST_CLOSE_TOKEN."
".self::TRUST_OPEN_TOKEN."div id='agree-button-done'".self::TRUST_CLOSE_TOKEN."Thank you for contributing.".self::TRUST_OPEN_TOKEN."/div".self::TRUST_CLOSE_TOKEN."
";
   }

   // Return if trust is not selected.
   if (!$this->trust_engaged)
     return true;
   
   // Save the title object, if it is not already present
   if (!$this->title){
     $this->title = $parser->getTitle();
   }
   
   // Load the current revision id.
   if (!$this->current_rev){
     if ($parser->mRevisionId){
       $this->current_rev = $parser->mRevisionId;
     } else {
       // Sometimes the revisionId field is not filled in.
       $this->current_rev = $this->title->getPreviousRevisionID( PHP_INT_MAX );
     }
   }
  
   /**
    This method is being called multiple times for each page. 
    We only pull the colored text for the first time through.
   */
   if ($this->colored){
     return true;
   }
   
   if ($wgRequest->getVal('diff')){
     $this->times_rev_loaded++;
     // For diffs, look for the absence of the diff token instead of counting
     if(substr($text,0,3) == self::DIFF_TOKEN_TO_COLOR){
       return true;
     }
   } 
   
   // if we made it here, we are going to color some text
   $this->colored = true;
   
   // Otherwise, see if there is colored text in the db.
   $dbr =& wfGetDB( DB_SLAVE );
   
   $res = $dbr->select('wikitrust_colored_markup', 'revision_text',
		       array( 'revision_id' => $this->current_rev ), array());
   if ($res){
     $row = $dbr->fetchRow($res);
     $colored_text = $row[0];
     if ($colored_text){
       $text = $iLikeItText . $colored_text . "\n" . $wgTrustExplanation;
     } else { 
       // If colored text does not exist, we start a coloring that explicitly requests
       // the uncolored revision to be colored.  This is useful in case there are holes
       // in the chronological order of the revisions that have been colored. 
       $command = "nohup $wgTrustCmd -rev_id " . $this->current_rev . " -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname >> $wgTrustDebugLog 2>&1 & echo $!";
       $pid = shell_exec($command);
     }
   } else {
     return false;
   }
   
   $dbr->freeResult( $res );
   return true;
 }
 
 /* Register the tags we are intersted in expanding. */
 function ucscColorTrust_Magic( &$magicWords, $langCode ) {
   $magicWords[ 't' ] = array( 0, 't' );
   return true;
 }
 
 /* Turn the finished trust info into a span tag. Also handle closing tags. */
 function ucscOrigin_Finalize(&$parser, &$text) {
   $count = 0;
   $text = preg_replace('/' . self::TRUST_OPEN_TOKEN . '/', "<", $text, -1, $count);
   $text = preg_replace('/' . self::TRUST_CLOSE_TOKEN .'/', ">", $text, -1, $count);
   $text = preg_replace('/<\/p>/', "</span></p>", $text, -1, $count);
   $text = preg_replace('/<p><\/span>/', "<p>", $text, -1, $count);
   $text = preg_replace('/<li><\/span>/', "<li>", $text, -1, $count);

  
   return true;
 }

 /* Text Trust */
 function ucscColorTrust_Render( &$parser, $combinedValue = "0,0" ) {
   
   // Split the value into trust and origin information.
   // 0 = trust
   // 1 = origin
   $splitVals = explode(self::TRUST_SPLIT_TOKEN, $combinedValue);
   
   $class = $this->computeColorFromFloat($splitVals[0]);
   $output = self::TRUST_OPEN_TOKEN . "span class=\"$class\" onclick=\"showOrigin(" 
     . $splitVals[1] . ")\"" . self::TRUST_CLOSE_TOKEN;

   $this->current_trust = $class;
   if ($this->first_span){
     $this->first_span = false;
   } else {
     $output = self::TRUST_OPEN_TOKEN . "/span" . self::TRUST_CLOSE_TOKEN . $output;
   }
   
   return array ( $output, "noparse" => false, "isHTML" => false );
 }
 
 /** 
  Maps from the online trust values to the css trust values.
  Normalize the value for growing wikis.
 */
 function computeColorFromFloat($trust){
   $normalized_value = min(self::MAX_TRUST_VALUE, max(self::MIN_TRUST_VALUE, 
						      ($trust * self::TRUST_MULTIPLIER) 
						      / $this->median));
   return $this->computeColor3($normalized_value);
 }
 
 /* Maps a trust value to a HTML color representing the trust value. */
 function computeColor3($fTrustValue){
   return $this->COLORS[$fTrustValue];
 }
}    

TextTrust::singleton();

?>
