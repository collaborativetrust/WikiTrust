#!/usr/bin/php
<?php

# Copyright (c) 2007,2008 Luca de Alfaro
# Copyright (c) 2007,2008 Ian Pye
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

# This script created the wikitrust tables needed for an online analysis.
# Usage: ./create_db.php "path to the target MediaWiki installation" 
#                         database_root_user 
#                         [remove]

$mw_root = $argv[1];
$dba = $argv[2];
$dba_pass = "";

// If true, we remove tables. If false, we create them.
$do_remove = ($argc > 2 && $argv[3] == "remove")? true: false; 

$db_tables = array(); // Store all of the tables currently present.
$create_scripts = array(); // The actual SQL to create tables. Defined below.
$remove_scripts = array(); // The actual SQL to remove tables. Defined below.

if(!$mw_root || !is_dir($mw_root) || !isset($dba)
   || !is_file($mw_root."/LocalSettings.php")){
  print "Usage: ./create_db.php 'path to the target MediaWiki installation' database_root_user [remove]\n";
  exit(-1);
 }

print ($do_remove)? "Removing tables\n": "Creating tables\n";
print "Do you really want to do this? [Y/n]: ";
$continue = strtoupper(fread(STDIN, 1));
if ($continue != "Y" && $continue != "\n"){
  print "Aborting script\n";
  exit(0);
 }

// Reads the root password from std in.
print "Enter the root mysql password:\n";
$dba_pass = rtrim(shell_exec('
bash -c \'
stty_orig=`stty -g`
trap "stty ${stty_orig}; exit" 1 2 3 15
stty -echo <&- 2<&-
read -s PASS
stty ${stty_orig} <&- 2<&-
trap 1 2 3 15
echo $PASS
\'
'));

// Load all of the MW files.
include($mw_root."/maintenance/commandLine.inc");

global $wgDBserver, $wgDBname, $wgDBuser;

// Create the needed tables, if neccesary.
$dbr =& wfGetDB( DB_SLAVE );

// First check to see what tables have already been created.
$res = $dbr->query("show tables");
while ($row = $dbr->fetchRow($res)){
  $db_tables[$row[0]] = True;
 }

// We need root priveledges to do this.
$db_root = Database::newFromParams($wgDBserver, $dba, $dba_pass, $wgDBname);

// These scripts hold the SQL to create and remove tables.
$create_scripts['wikitrust_global'] = 
  array("
CREATE TABLE wikitrust_global (
       median		          float,
       rep_0			  float,
       rep_1			  float,
       rep_2			  float,
       rep_3			  float,
       rep_4			  float,
       rep_5			  float,
       rep_6			  float,
       rep_7			  float,
       rep_8			  float,
       rep_9			  float
) ENGINE=InnoDB",
	"GRANT ALL ON wikitrust_global TO $wgDBuser",
	"INSERT INTO wikitrust_global VALUES (0,0,0,0,0,0,0,0,0,0,0)");

$create_scripts['wikitrust_page'] = array("
CREATE TABLE wikitrust_page (
       page_id             int PRIMARY KEY,
       deleted_chunks      longtext,
       page_info	   text NOT NULL
) ENGINE=InnoDB","
GRANT ALL ON wikitrust_page TO $wgDBuser
");

$create_scripts['wikitrust_revision'] = array("
CREATE TABLE wikitrust_revision (
        revision_id             int PRIMARY KEY,
        quality_info		text NOT NULL, 
	reputation_delta        float DEFAULT 0.0,
	overall_trust           float DEFAULT 0.0
) ENGINE=InnoDB","
GRANT ALL ON wikitrust_revision TO $wgDBuser;
");

$create_scripts['wikitrust_colored_markup'] = array("
CREATE TABLE wikitrust_colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   longtext NOT NULL,
	revision_createdon varchar(32) NOT NULL
) ENGINE=InnoDB","
GRANT ALL ON wikitrust_colored_markup TO $wgDBuser","
CREATE INDEX wikitrust_colored_markup_createdon_idx ON wikitrust_colored_markup (revision_createdon)
");

$create_scripts['wikitrust_sigs'] = array("
CREATE TABLE wikitrust_sigs (
       revision_id      int PRIMARY KEY,
       words		longtext NOT NULL,
       trust            longtext NOT NULL,
       origin           longtext NOT NULL,
       sigs     	longtext NOT NULL
) ENGINE=InnoDB","
GRANT ALL ON wikitrust_sigs TO $wgDBuser;
");

$create_scripts['wikitrust_user'] = array("
CREATE TABLE wikitrust_user (
       user_id     int PRIMARY KEY   ,
       user_rep    float DEFAULT 0.0
) ENGINE=InnoDB","
GRANT ALL ON wikitrust_user TO $wgDBuser;
");


$remove_scripts['wikitrust_global'] = array("DROP TABLE wikitrust_global");
$remove_scripts['wikitrust_page'] = array("DROP TABLE wikitrust_page");
$remove_scripts['wikitrust_revision'] = array("DROP TABLE wikitrust_revision");
$remove_scripts['wikitrust_colored_markup'] = array("DROP TABLE wikitrust_colored_markup");
$remove_scripts['wikitrust_sigs'] = array("DROP TABLE wikitrust_sigs");
$remove_scripts['wikitrust_user'] = array("DROP TABLE wikitrust_user");

if (!$do_remove){
  // Now do the actual creating.
  foreach ($create_scripts as $table => $scripts) {
    if (!$db_tables[$table]){
      foreach ($scripts as $script){
	$db_root->query($script);
      }
    }
  }
 } else {
  // Or removing.
  foreach ($remove_scripts as $table => $scripts) {
    if ($db_tables[$table]){
      foreach ($scripts as $script){
	$db_root->query($script);
      }
    }
  }
 }

// Finally, we commit any leftovers.
$db_root->query("COMMIT");

print ($do_remove)? "Removed tables\n": "Created tables\n";

?>
