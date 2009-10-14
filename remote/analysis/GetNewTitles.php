<?php

header('Content-type: text/plain');
require_once('../database.inc');

$db=$_GET['db'];
if(!$db){
  print("db required");
  exit(0);
}

mysql_connect($db.'-p.db.ts.wikimedia.org',$toolserver_username,$toolserver_password);
@mysql_select_db($db.'_p') or print mysql_error();

$query = sprintf("SELECT page_id, page_title FROM page WHERE page_latest >= '%s' AND page_namespace = 0", 
	mysql_real_escape_string($_GET['n']));
$result = mysql_query($query);
 
$data = array();
while($row = mysql_fetch_assoc($result))
{
  array_push($data, $row);
}

print(json_encode($data));
       
mysql_close();

?>
