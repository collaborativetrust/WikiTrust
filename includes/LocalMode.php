<?php

class WikiTrust extends WikiTrustBase {

    static function ucscOutputBeforeHTML(&$out, &$text) {
	super::ucscOutputBeforeHTML($out, $text);
	trustdata_loadFDb();
	return true;
    }


    /** 
     * Actually run the eval edit program.
     * Returns -1 on error, the process id of the launched eval process 
     otherwise.
    */
    private static function runEvalEdit($eval_type = self::TRUST_EVAL_EDIT,
				$rev_id = -1, $page_id = -1,
				$voter_id = -1)
    {
	global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgTrustCmd, $wgTrustLog, $wgTrustDebugLog, $wgRepSpeed, $wgDBprefix, $wgThrift_host, $wgThrift_port, $wgThrift_uri, $wgThrift_protocol;
	    
	$process = -1;
	$command = "";
	// Get the db.
	$dbr =& wfGetDB( DB_SLAVE );
	    
	// Do we use a DB prefix?
	$prefix = ($wgDBprefix)? "-db_prefix " . $dbr->strencode($wgDBprefix): "";
	    
	switch ($eval_type) {
	    case self::TRUST_EVAL_EDIT:
		$command = escapeshellcmd("$wgTrustCmd -rep_speed $wgRepSpeed -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname -thrift_host $wgThrift_host -thrift_port $wgThrift_port -thrift_uri $wgThrift_uri -thrift_protocol $wgThrift_protocol $prefix") . " &";
		break;
	    case self::TRUST_EVAL_VOTE:
		if ($rev_id == -1 || $page_id == -1 || $voter_id == -1)
		    return -1;
		$command = escapeshellcmd("$wgTrustCmd -eval_vote -rev_id " . $dbr->strencode($rev_id) . " -voter_id " . $dbr->strencode($voter_id) . " -page_id " . $dbr->strencode($page_id) . " -rep_speed $wgRepSpeed -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname -thrift_host $wgThrift_host -thrift_port $wgThrift_port -thrift_uri $wgThrift_uri -thrift_protocol $wgThrift_protocol $prefix") . " &";
		break;
	    case self::TRUST_EVAL_MISSING:
		$command = escapeshellcmd("$wgTrustCmd -rev_id " . $dbr->strencode($rev_id) . " -rep_speed $wgRepSpeed -log_file $wgTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname -thrift_host $wgThrift_host -thrift_port $wgThrift_port -thrift_uri $wgThrift_uri -thrift_protocol $wgThrift_protocol $prefix") . " &";
		break;  
	}
	    
	$descriptorspec = array(
		0 => array("pipe", "r"),
		1 => array("file", escapeshellcmd($wgTrustDebugLog), "a"),
		2 => array("file", escapeshellcmd($wgTrustDebugLog), "a")
	    );
	$cwd = '/tmp';
	$env = array();
	$process = proc_open($command, $descriptorspec, $pipes, $cwd, $env);

	return $process; 
    }

}

?>
