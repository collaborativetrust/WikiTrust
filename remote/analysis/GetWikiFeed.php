<?php

$SERVER = "http://toolserver.org/~ipye/GetNewTitles.php?";
$mwdb = $argv[1];
$wikidb = $argv[2];
$wikiuser = $argv[3];
$wikipass = $argv[4];

if(!isset($mwdb) || !$wikiuser || !$wikipass){
  print "Usage: php GetWikiFeed db dbname dbuser dbpass\n";
  exit(-1);
}

// Connect to the db
$db = mysql_connect("localhost", $wikiuser, $wikipass);
mysql_select_db($wikidb, $db);

// Get the latest rev we have
$query = "SELECT revision_id FROM wikitrust_revision ORDER BY revision_id DESC LIMIT 1; ";
$result = mysql_query($query);
if (!$result) {
    $message  = 'Invalid query: ' . mysql_error() . "\n";
    $message .= 'Whole query: ' . $query;
    die($message);
}

$last_rev = 0;
if ($row = mysql_fetch_assoc($result)) {
  $last_rev = $row['revision_id'];
}
mysql_free_result($result);

//$last_rev="344402323";

// And now get all of the revs since then
$json_raw = file_get_contents($SERVER."db=$mwdb&n=$last_rev");
$json_parsed = json_decode($json_raw, TRUE);

// And finally put them into the db
foreach ($json_parsed as $key => $value){
  mysql_query("INSERT INTO wikitrust_queue (page_id, page_title) VALUES (".
      mysql_real_escape_string($value['page_id']).",'"
      .mysql_real_escape_string($value['page_title'])."')"
      ."ON DUPLICATE KEY UPDATE requested_on = now();");
}

mysql_close($db);
?>

