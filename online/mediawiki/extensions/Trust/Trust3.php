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

## the css tag to use
$TRUST_CSS_TAG = "background-color"; ## color the background
#$TRUST_CSS_TAG = "color"; ## color just the text

## Path to eval_online_wiki
$EVAL_ONLINE_WIKI = "/home/ipye/git/wikitrust/online/analysis/eval_online_wiki";

## Hard coded coloring arguments
$EVAL_ONLINE_WIKI_ARGS = '-db_user wikiuser -db_pass wikiword -db_name wikidb1 -log_name /tmp/color-on-edit.log';

## We only want to run one coloring process at a time
$EVAL_ONLINE_LOCK_FILE = "/tmp/mw_coloring.lock";

## Median file cache lives here
$EVAL_MEDIAN_REP_FILE = "/tmp/mw_median_rep";

## Log file for the coloring process
$EVAL_ONLINE_LOG_FILE = "/tmp/coloring.log";

## Trust normalization values;
$MAX_TRUST_VALUE = 9;
$MIN_TRUST_VALUE = 0;
$TRUST_MULTIPLIER = 10;

## Poll the median value this often
$MW_RAND_MIN = 0;
$MW_RAND_MAX = 100;
$MW_POLL_UNDER = 10;


## map trust values to html color codes
$COLORS = array(
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

# Credits
$wgExtensionCredits['parserhook'][] = array(
					    'name' => 'Trust Coloring',
					    'author' =>'Ian Pye', 
					    'url' => 
					    'http://trust.cse.ucsc.edu', 
					    'description' => 'This Extension 
colors text according to trust.'
       );

# Define a setup function
$wgExtensionFunctions[] = 'ucscColorTrust_Setup';

# Add a hook to initialise the magic word
$wgHooks['LanguageGetMagic'][] = 'ucscColorTrust_Magic';

# And add a hook so the colored text is found. 
$wgHooks['ParserBeforeStrip'][] = 'ucscSeeIfColored';

# And add and extra tab.
$wgHooks['SkinTemplateTabs'][] = 'ucscTrustTemplate';

# Color saved text
$wgHooks['ArticleSaveComplete'][] = 'ucscRunColoring';

/**
 Updated the cached median reputation value.
 */
function update_median($median_file){
  $dbr =& wfGetDB( DB_SLAVE );
  $res = $dbr->select('wikitrust_histogram', 'median', array(), array());
  if ($res){
    $row = $dbr->fetchRow($res);
    $median = $row['median'];
    if ($median){
      file_put_contents($median_file, $median);
    } 
  } 
  $dbr->freeResult( $res );
  return $median;
}

/* 
 Code to fork and exec a new process to color any new revisions.
 Called after any edits are made.

 This assums that the php process has write access to /tmp.
*/
function ucscRunColoring(&$article, &$user, &$text, &$summary, $minor, $watch, $sectionanchor, &$flags, $revision) { 
  global $EVAL_ONLINE_WIKI;
  global $EVAL_ONLINE_WIKI_ARGS;
  global $EVAL_ONLINE_LOCK_FILE;
  global $EVAL_ONLINE_LOG_FILE;
  global $MW_RAND_MIN;
  global $MW_RAND_MAX;
  global $MW_POLL_UNDER;

  $pid = -1;

  // Update the cached median reputation info value, but only sometimes.
  if (rand($MW_RAND_MIN, $MW_RAND_MAX) < $MW_POLL_UNDER){
    update_median($EVAL_MEDIAN_REP_FILE);
  }
  
  // We don't want more than one copy of the coloring going at any one time.
  if (file_exists($EVAL_ONLINE_LOCK_FILE)){
    $pid = file_get_contents($EVAL_ONLINE_LOCK_FILE);
    exec("ps $pid", $pState); // Is the process still running?
    if ((count($pState) >= 2)){
      return true;
    }
  } 

  // Start the coloring since no other coloring processes are going.
  $pid = shell_exec("nohup $EVAL_ONLINE_WIKI $EVAL_ONLINE_WIKI_ARGS >> $EVAL_ONLINE_LOG_FILE 2>&1 & echo $!");

  // And mark that we started this process.
  file_put_contents($pid);
  
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
 If colored text exists, use it instead of the normal text.
 TODO: make this optional.
 */
function ucscSeeIfColored(&$parser, &$text, &$strip_state) { 
  global $EVAL_MEDIAN_REP_FILE;
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

  /* This is being called multiple times for each page. The upshot of
   this is that the first time is good, and after that the footer is
   gettting messed up. This is desinged in an EXTREMELY hacky manor
   to avoid this. */
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

function ucscColorTrust_Setup() {
  global $wgParser;
  # Set a function hook associating the "example" magic word with our function
  $wgParser->setFunctionHook( 't', 'ucscColorTrust_Render'  );
  $wgParser->setFunctionHook( 'to', 'ucscOrigin_Render', SFH_NO_HASH );
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
  $class = computeColorFromFloat($value);
  $output = "<span class=$class>";
  return array ( $output, "noparse" => false, "isHTML" => false );
}

## Maps from the online trust values to the css trust values.
## Normalize the value for growing wikis.
function computeColorFromFloat($trust){
  global $EVAL_MEDIAN_REP_FILE;
  global $MAX_TRUST_VALUE;
  global $MIN_TRUST_VALUE;
  global $TRUST_MULTIPLIER;
  
  $median = 0.0;
  if( file_exists($EVAL_MEDIAN_REP_FILE)){
    $median = floatval(file_get_contents($EVAL_MEDIAN_REP_FILE));
  } else {
    $median = update_median($EVAL_MEDIAN_REP_FILE);
  }
  $normalized_value = min($MAX_TRUST_VALUE, max($MIN_TRUST_VALUE, 
						($trust * $TRUST_MULTIPLIER) 
						/ $median));
  return computeColor3($normalized_value);
}

## this function maps a trust value to a HTML color representing the trust value
function computeColor3($fTrustValue){
  global $COLORS;
  return $COLORS[$fTrustValue];
}

?>
