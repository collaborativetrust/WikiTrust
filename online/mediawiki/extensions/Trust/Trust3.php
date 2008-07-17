<?php

# Copyright (c) 2007 Luca de Alfaro
# Copyright (c) 2007 Ian Pye
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
$wgExtensionFunctions[] = 'wfColorTrust_Setup';

# Add a hook to initialise the magic word
$wgHooks['LanguageGetMagic'][] = 'wfColorTrust_Magic';

# And add a hook so the colored text is found. 
$wgHooks['ParserBeforeStrip'][] = 'ucscSeeIfColored';

$wgHooks['SkinTemplateTabs'][] = 'ucscTrustTemplate';

function ucscTrustTemplate($skin, &$content_actions) { 
  $content_actions['trust'] = array ( 'class' => '',
				      'text' => 'Trust',
				      'href' => $content_actions['nstab-main']['href'] . "?trust=1" );
  return true;
}

/**
 If colored text exists, use it instead of the normal text.
 TODO: make this optional.
 */
function ucscSeeIfColored(&$parser, &$text, &$strip_state) { 

  //if($_GET['trust'] == ""){
  //  print $_GET['trust'];
  //  return true;
  // }

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

function wfColorTrust_Setup() {
  global $wgParser;
  # Set a function hook associating the "example" magic word with our function
  $wgParser->setFunctionHook( 't', 'wfColorTrust_Render'  );
  $wgParser->setFunctionHook( 'to', 'wfOrigin_Render', SFH_NO_HASH );
}
 
function wfColorTrust_Magic( &$magicWords, $langCode ) {
  # Add the magic word
  # The first array element is case sensitive, in this case it is not case sensitive
  # All remaining elements are synonyms for our parser function
  $magicWords[ 't' ] = array( 0, 't' );
  $magicWords[ 'to' ] = array( 0, 'to' ); 
  # unless we return true, other parser functions extensions won't get loaded.
  return true;
}

function wfOrigin_Render( &$parser, $origin = 0 ) {
  $output = "<span onclick='showOrigin($origin)'>";     
  return array( $output, "noparse" => true, "isHTML" => false );  
}

## here, we are given a value and an optional start tag
## and return a string to take the place of the {{#trust tag
function wfColorTrust_Render( &$parser, $value = 0 ) {
  # The parser function itself
  # The input parameters are wikitext with templates expanded
  # The output should be wikitext too
  $class = computeColorFromFloat($value);
  $output = "<span class=$class>";
  return array ( $output, "noparse" => false, "isHTML" => false );
}

function computeColorFromFloat($value){
  $value = $value * 10;
  return computeColor3(intval($value));
}

## this function maps a trust value to a HTML color representing the trust value
function computeColor3($fTrustValue){
  global $COLORS;
  return $COLORS[$fTrustValue];
}

?>
