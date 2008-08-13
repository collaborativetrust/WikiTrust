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
  
  ## Trust normalization values;
  const MAX_TRUST_VALUE = 9;
  const MIN_TRUST_VALUE = 0;
  const TRUST_MULTIPLIER = 10;

  ## Median Value of Trust
  var $median = 0.0;
    
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
    
    global $wgHooks, $wgParser;
    
# Add a hook to initialise the magic word
    $wgHooks['LanguageGetMagic'][] = array( &$this, 'ucscColorTrust_Magic');
    
# And add a hook so the colored text is found. 
    $wgHooks['ParserBeforeStrip'][] = array( &$this, 'ucscSeeIfColored');
    
# And add and extra tab.
    $wgHooks['SkinTemplateTabs'][] = array( &$this, 'ucscTrustTemplate');
    
# Color saved text
    $wgHooks['ArticleSaveComplete'][] = array( &$this, 'ucscRunColoring');
   
# Set a function hook associating the "example" magic word with our function
    $wgParser->setFunctionHook( 't', array( &$this, 'ucscColorTrust_Render'));
    $wgParser->setFunctionHook( 'to', array( &$this, 'ucscOrigin_Render'), SFH_NO_HASH );

# Pull the median value
    $this->update_median();
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
   return $this->median;
 }
 
/* 
 Code to fork and exec a new process to color any new revisions.
 Called after any edits are made.
*/
 function ucscRunColoring(&$article, &$user, &$text, &$summary, $minor, $watch, $sectionanchor, &$flags, $revision) { 
   global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgTrustCmd, $wgTrustLog, $wgTrustDebugLog;
   
   $pid = -1;
   
   // Start the coloring.
   $command = "nohup $wgTrustCmd -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname >> $wgTrustDebugLog 2>&1 & echo $!";
   // $pid = shell_exec("/bin/echo '$command' >> $wgTrustDebugLog");
   $pid = shell_exec($command);
  
   if($pid)
     return true;
   return false;
 }

# Actually add the tab.
 function ucscTrustTemplate($skin, &$content_actions) { 
  
   $trust_qs = $_SERVER['QUERY_STRING'];
   if($trust_qs){
     $trust_qs = "?" . $trust_qs .  "&trust=t";
   } else {
     $trust_qs .= "?trust=t"; 
   }
   
   $content_actions['trust'] = array ( 'class' => '',
				       'text' => 'Trust',
				       'href' => 
				       $_SERVER['PHP_SELF'] . $trust_qs );
   
   if(isset($_GET['trust'])){
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
*/
 function ucscSeeIfColored(&$parser, &$text, &$strip_state) { 
   global $wgParserCacheType;
   // disable the parser cache for this transaction.
   // Hacked up from code found in the ParserCacheControl extension
   // http://www.bluecortex.com
   $wgParserCacheType = CACHE_NONE;
   $apc =& wfGetParserCacheStorage();
   
   $pc = & ParserCache::singleton();
   $pc->mMemc = $apc;
   
  // If we don't want colored text, return true immediatly.
   if(!isset($_GET['trust'])){
    return true;
   } 

   /**
   This method is being called multiple times for each page. The upshot of
   this is that the first time it is invoked, everything is good.
   After that though the footer is gettting messed up.
   To avoid this, we test to see if we are dealing with a footer, and only
   proceed if we are not.
  */
   $pos1 = strpos($text, "{{SITENAME}}");    
   $pos2 = strpos($text, "{{PLURAL");
   if($pos1 !== false || $pos2 !== false){
     return true;
   }
   
   // Otherwise, see if there is colored text in the db.
   $dbr =& wfGetDB( DB_SLAVE );
   $rev_id = $parser->mRevisionId;
  
   $res = $dbr->select('wikitrust_colored_markup', 'revision_text',
		       array( 'revision_id' => $rev_id ), array());
   if ($res){
     $row = $dbr->fetchRow($res);
     $colored_text = $row['revision_text'];
     if ($colored_text){
       $text = $colored_text;
     }
   } 
   
   $dbr->freeResult( $res );
   return true;
}
 
 function ucscColorTrust_Magic( &$magicWords, $langCode ) {
# Add the magic word
# The first array element is case sensitive, in this case it is not case sensitive
# All remaining elements are synos, $langCode ) {
# Add the magic word
# The first array element is case sensitive, in this case it is not case sensitive
# All remaining elements are synonyms for our parser function
   $magicWords[ 't' ] = array( 0, 't' );
   $magicWords[ 'to' ] = array( 0, 'to' ); 
  # unless we return true, other parser functions extensions won't get loaded.
   return true;
 }

 function ucscOrigin_Render( &$parser, $origin = 0 ) {
   $output = "<span onclick='showOrigin($origin)'>";     
   return array( $output, "noparse" => true, "isHTML" => false );  
 }

 ## here, we are given a value and an optional start tag
 ## and return a string to take the place of the {{#trust tag
 function ucscColorTrust_Render( &$parser, $value = 0 ) {
# The parser function itself
# The input parameters are wikitext with templates expanded
# The output should be wikitext too
   $class = $this->computeColorFromFloat($value);
   $output = "<span class=$class>";
   return array ( $output, "noparse" => false, "isHTML" => false );
 }
 
## Maps from the online trust values to the css trust values.
## Normalize the value for growing wikis.
 function computeColorFromFloat($trust){
   
   $normalized_value = min(self::MAX_TRUST_VALUE, max(self::MIN_TRUST_VALUE, 
						      ($trust * self::TRUST_MULTIPLIER) 
						      / $this->median));
   return $this->computeColor3($normalized_value);
 }
 
 ## this function maps a trust value to a HTML color representing the trust value
 function computeColor3($fTrustValue){
   return $this->COLORS[$fTrustValue];
 }
}    

TextTrust::singleton();

?>
