<?php
## Load the following extensions for us
require_once( $IP . "/extensions/Trust/Trust3.php" );
require_once( $IP . "/extensions/Trust/NotifyEdit.php" );

## Load the other common ones
require_once("$IP/extensions/ParserFunctions/ParserFunctions.php");

$wgUseTidy = true;

## Citations
require_once("$IP/extensions/Cite/Cite.php");

## Category Trees
$wgUseAjax = true;
require_once( "{$IP}/extensions/CategoryTree/CategoryTree.php" );

## wikihero ??
require_once("$IP/extensions/wikihiero/wikihiero.php");

## math
$wgUseTeX = true;

## ImageMap
require_once( "$IP/extensions/ImageMap/ImageMap.php" );

require_once( "$IP/extensions/AntiSpoof/AntiSpoof.php" );

require_once( "$IP/extensions/CharInsert/CharInsert.php" );

require_once( "$IP/extensions/Filepath/SpecialFilepath.php" );

require_once( "$IP/extensions/Gadgets/Gadgets.php" );

require_once( "$IP/extensions/LabeledSectionTransclusion/lst.php" );

require_once( "$IP/extensions/Poem/Poem.php" );

require_once( "$IP/extensions/Quiz/Quiz.php" );

require_once( "$IP/extensions/SyntaxHighlight_GeSHi/SyntaxHighlight_GeSHi.php" );
