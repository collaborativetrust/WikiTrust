<?php

class WikiTrustBase {
  ## Cache time that results are valid for.  Currently 1 day.
  const TRUST_CACHE_VALID = 31556926;

  ## Types of analysis to perform.
  const TRUST_EVAL_VOTE = 0;
  const TRUST_EVAL_EDIT = 10;

  ## Trust normalization values;
  const MAX_TRUST_VALUE = 9;
  const MIN_TRUST_VALUE = 0;
  const TRUST_MULTIPLIER = 10;
  
  ## Token to be replaed with <
  const TRUST_OPEN_TOKEN = "QQampo:";
  
  ## Token to be replaed with >
  const TRUST_CLOSE_TOKEN = ":ampc:";

  ## Server forms
  const NOT_FOUND_TEXT_TOKEN = "TEXT_NOT_FOUND";

  ## Context for communicating with the trust server
  const TRUST_TIMEOUT = 10;

  ## Default median to avoid div by 0 errors
  const TRUST_DEFAULT_MEDIAN = 1;

  ## Median Value of Trust
  static $median = 1.0;

  ## Don't close the first opening span tag
  static $first_span = true;

  ## Stores the colored text between function calls
  static $colored_text = "";

  ## Has the colored text been loaded?
  static $colored_text_loaded = false;
    
  ## map trust values to html color codes
  static $COLORS = array(
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


	
  /**
   Records the vote.
   Called via ajax, so this must be static.
  */
  static function handleVote($user_name_raw, $page_id_raw = 0, 
			     $rev_id_raw = 0, $page_title_raw = "")
  {
    
    wfWikiTrustDebug(__FILE__.":".__LINE__
        . ": Handling vote from $user_name_raw $page_title_raw");
    $dbr =& wfGetDB( DB_SLAVE );
    
    $userName = $dbr->strencode($user_name_raw, $dbr);
    $page_id = $dbr->strencode($page_id_raw, $dbr);
    $rev_id = $dbr->strencode($rev_id_raw, $dbr);
    $pageTitle = $dbr->strencode($page_title_raw, $dbr);

    if(!$page_id || !$userName || !$rev_id || !$pageTitle)
      return new AjaxResponse("0");

    $user_id = self::user_getIdFName($dbr, $userName);
    if(!$user_id)
      $user_id = 0;
    wfWikiTrustDebug(__FILE__.":".__LINE__.": UserId: $user_id");

    return WikiTrust::vote_recordVote($dbr, $user_id, $page_id, $rev_id, $pageTitle);
  }


  static function vote_recordVote(&$dbr, $user_id, $page_id, $rev_id, $pageTitle)
  {
    // Now see if this user has not already voted, 
    // and count the vote if its the first time though.
    $res = $dbr->select('wikitrust_vote', array('revision_id'),
		array('revision_id' => $rev_id, 'voter_id' => $user_id),
		array());
    if (!$res || $dbr->numRows($res) == 0) return new AjaxResponse("0");
	// TODO: do we also need to $dbr->freeResult($res)?

    $row = $dbr->fetchRow($res);
    $dbr->freeResult($res);
    if ($row['revision_id']) return new AjaxResponse("Already Voted");

    $insert_vals = array(
       "revision_id" => $rev_id,
			 "page_id" => $page_id ,
			 "voter_id" => $user_id,
			 "voted_on" => wfTimestampNow()
		   );
    $dbw =& wfGetDB( DB_MASTER );
    if ($dbw->insert( 'wikitrust_vote', $insert_vals)) {
      $dbw->commit();
      self::runEvalEdit(self::TRUST_EVAL_VOTE, $rev_id, $page_id, $user_id); 
      return new AjaxResponse(implode  ( ",", $insert_vals));
    } else {
      return new AjaxResponse("0");
    }
  }
  
  static function ucscOutputBeforeHTML(&$out, &$text){
    wfLoadExtensionMessages('WikiTrust');

    self::color_addFileRefs($out);
    $rev_id = self::util_getRevFOut($out);
    $colored_text = self::$colored_text;
    if (!self::$colored_text_loaded){
      $colored_text = WikiTrust::color_getColorData($rev_id);
      self::color_fixup($colored_text);
    }

    if (!$colored_text) {
      wfWikiTrustDebug(__FILE__ . ":"
          . __LINE__ . " $rev_id: colored text not found.");
      // text not found.
      global $wgUser, $wgParser, $wgTitle;
      $options = ParserOptions::newFromUser($wgUser);
      $msg = $wgParser->parse(wfMsgNoTrans("wgNoTrustExplanation"), 
				$wgTitle, 
				$options);
      $text = $msg->getText() . $text;
    } else {
      self::color_Wiki2Html($colored_text, $text);
      self::vote_showButton($text);
    }

    return true;
  }

  static function color_addFileRefs(&$out) {
    global $wgScriptPath;
    $out->addScript("<script type=\"text/javascript\" src=\""
		.$wgScriptPath
		."/extensions/WikiTrust/js/trust.js\"></script>");
    $out->addScript("<link rel=\"stylesheet\" type=\"text/css\" href=\""
		.$wgScriptPath."/extensions/WikiTrust/css/trust.css\">"); 
  }

  static function vote_showButton(&$text)
  {
    global $wgWikiTrustShowVoteButton, $wgUseAjax;

    if ($wgWikiTrustShowVoteButton && $wgUseAjax){
      $text = "<div id='vote-button'><input type='button' name='vote' "
		. "value='" 
		. wfMsgNoTrans("wgTrustVote")
		. "' onclick='startVote()' /></div><div id='vote-button-done'>"
		. wfMsgNoTrans("wgTrustVoteDone") 
		. "</div>"
		. $text;
    }
  }

  static function voteToProcess($rev_id)
  {
    $voteToProcess = false;
    $dbr =& wfGetDB( DB_SLAVE );
    $res = $dbr->select('wikitrust_vote', 'processed',
			array( 'revision_id' => $rev_id ), 
      array());
    if ($res && $dbr->numRows($res) > 0) {
      $row = $dbr->fetchRow($res);
      if (!$row[0]){
        $voteToProcess = true;
      }
    }
    $dbr->freeResult( $res ); 
    return $voteToProcess;
  }

  static function color_getColorData($rev_id)
  {
    wfWikiTrustDebug(__FILE__.":".__LINE__ . ": " . __METHOD__);
    if (!$rev_id)
      return '';

    $colored_text = "";

    $dbr =& wfGetDB( DB_SLAVE );

    wfWikiTrustDebug(__FILE__.":".__LINE__ . ": Looks in the database.");

    global $wgWikiTrustColorPath;
    if (!$wgWikiTrustColorPath) {
      $res = $dbr->select('wikitrust_colored_markup', 'revision_text',
			array( 'revision_id' => $rev_id ), 
			array());
      if (!$res || $dbr->numRows($res) == 0) {
	wfWikiTrustDebug(__FILE__.":".__LINE__ . ": Calls the evaluation.");
	self::runEvalEdit(self::TRUST_EVAL_EDIT);
	return '';
      }
      wfWikiTrustDebug(__FILE__.":".__LINE__ . ": It thinks it has found colored text.");
      $row = $dbr->fetchRow($res);
      $colored_text = $row[0];
      $dbr->freeResult( $res ); 
    } else {
      global $wgTitle;
      $page_id = $wgTitle->getArticleID();
      $file = self::util_getRevFilename($page_id, $rev_id);
      // TODO: Bo -- what is this all about?
if (1) {
      $gzdata = @file_get_contents($file, FILE_BINARY, NULL);
} else {
      $gzdata = '';
      $fh = @fopen($file, "r");
      if ($fh) {
	$gzdata = fread($fh, filesize($file));
	fclose($fh);
      }
}
      if (!$gzdata) {
	self::runEvalEdit(self::TRUST_EVAL_EDIT);
	return '';
      }
      $colored_text = gzinflate(substr($gzdata, 10));
    }

    $res = $dbr->select('wikitrust_global', 'median', array(), array());
    if ($res && $dbr->numRows($res) > 0) {
      $row = $dbr->fetchRow($res);
      self::$median = $row[0];
      if ($row[0] == 0) {
	self::$median = self::TRUST_DEFAULT_MEDIAN;
      }
    }
    $dbr->freeResult( $res ); 

    return $colored_text;
  }

  static function color_Wiki2Html(&$colored_text, &$text)
  {
    global $wgParser, $wgUser, $wgTitle;
    $count = 0;

if(0) {
    // #t might be reserved already!!
    // TODO: big hack!!
    // TODO: This gets thrown away.  Safe to delete?  -Bo
    $text = preg_replace_callback("/\{\{#t:(\d+),(\d+),(.*?)\}\}/",
				"WikiTrust::color_t2trust",
				$text,
				-1,
				$count);
}

    $options = ParserOptions::newFromUser($wgUser);
    $parsed = $wgParser->parse($colored_text, $wgTitle, $options);
    $text = $parsed->getText();

    // Update the trust tags
    $text = preg_replace_callback("/\{\{#t:(\d+),(\d+),(.*?)\}\}/",
				"WikiTrust::color_handleParserRe",
				$text,
				-1,
				$count);
      
    // Update open, close, images, and links.
    $text = preg_replace('/' . self::TRUST_OPEN_TOKEN . '/', 
			 "<", $text, -1, $count);  
    $text = preg_replace('/' . self::TRUST_CLOSE_TOKEN .'/', 
			 ">", $text, -1, $count);
    $text = preg_replace('/<\/p>/', "</span></p>", $text, -1, $count);
    $text = preg_replace('/<p><\/span>/', "<p>", $text, -1, $count);
    $text = preg_replace('/<li><\/span>/', "<li>", $text, -1, $count);
    $text = preg_replace('/<\/dd><\/dl>/', "</span></dd></dl><span>", $text, -1, $count);

    // Fix edit section links
    $text = preg_replace_callback("/<span class=\"editsection\">\[(.*?)Edit section: <\/span>(.*?)\">edit<\/a>\]<\/span>/",
		"WikiTrust::color_handleFixSection",
		$text,
		-1,
		$count
	      );


    global $wgScriptPath;
    $text = '<script type="text/javascript" src="'
	      .$wgScriptPath
	      .'/extensions/WikiTrust/js/wz_tooltip.js"></script>' . $text;

    $msg = $wgParser->parse(wfMsgNoTrans("wgTrustExplanation"), 
			      $wgTitle, 
			      $options);
    $text .= $msg->getText();
  }

  static function color_handleParserRe($matches){
    
    $normalized_value = min(self::MAX_TRUST_VALUE, 
			    max(self::MIN_TRUST_VALUE, 
				(($matches[1] + .5) * 
				 self::TRUST_MULTIPLIER) 
				/ self::$median));
    $class = self::$COLORS[$normalized_value];
    $output = self::TRUST_OPEN_TOKEN . "span class=\"$class\"" 
      . " onmouseover=\"Tip('".str_replace("&#39;","\\'",$matches[3])
      ."')\" onmouseout=\"UnTip()\""
      . " onclick=\"showOrigin(" 
      . $matches[2] . ")\"" . self::TRUST_CLOSE_TOKEN;
    if (self::$first_span){
      self::$first_span = false;
    } else {
      $output = self::TRUST_OPEN_TOKEN . "/span" . self::TRUST_CLOSE_TOKEN . $output;
    }
    return $output;
  }

  static function color_t2trust($matches){
    return "{{#trust:".$matches[1].",".$matches[2].",".$matches[3]."}}";
  }


  static function color_handleFixSection($matches)
  {
    return "<span class=\"editsection\">[" .
	    $matches[1].
	    "Edit section: \">" .
	    "edit</a>]</span>";
  }

  static function color_fixup(&$colored_text)
  {
    // First, make sure that there are not any instances of our tokens in 
    // the colored_text
    $colored_text = str_replace(self::TRUST_OPEN_TOKEN, "", $colored_text);
    $colored_text = str_replace(self::TRUST_CLOSE_TOKEN, "", $colored_text);

    // TODO: I think these replacements are from broken XML parser?
	// Still needed?  (Luca working on fixing unpacking...) -Bo
    $colored_text = preg_replace("/&apos;/", "'", $colored_text, -1);      
    $colored_text = preg_replace("/&amp;/", "&", $colored_text, -1);
    $colored_text = preg_replace("/&lt;/", self::TRUST_OPEN_TOKEN, 
			       $colored_text, -1);
    $colored_text = preg_replace("/&gt;/", self::TRUST_CLOSE_TOKEN, 
			       $colored_text, -1);
  }
			

  public static function ucscArticleSaveComplete(&$article, 
			      &$user, &$text, &$summary,
			      &$minoredit, &$watchthis,
			      &$sectionanchor, &$flags, 
			      &$revision)
  {
    $page_id = $article->getTitle()->getArticleID();
    $rev_id = $revision->getID();
    $user_id = $user->getID();
		
    if (self::runEvalEdit(self::TRUST_EVAL_EDIT, 
			$rev_id, 
			$page_id,
			$user_id) >= 0) {
	return true;
    }
    return false;
  }

  // Cache control -- invalidate the cache if someone voted on the 
  // page recently, or if the colored page is invalid.
  // This function does this by making the sure the last
  // modified time of the page is set to now() if we don't want 
  // output cached.
  public static function ucscOutputModified(&$modified_times)
  {
    wfWikiTrustDebug(__FILE__.":".__LINE__
                     .": ".print_r($modified_times, true));
    
    // Load the colored text if the text is available.
    // We do it here because this is the first hook to be fired as a page
    // is rendered.
    // We don't need to check if colored_text is already present, because 
    // of this hook ordering.
    // We need to know if the colored text is missing or not, and just getting
    // it seems like the easiest way to figure this out.
    $rev_id = self::util_getRev();
    $colored_text = WikiTrust::color_getColorData($rev_id);
    self::color_fixup($colored_text);
    self::$colored_text = $colored_text;
    self::$colored_text_loaded = true;
    
    // Update the cache with the current time if we need to invalide it
    //   for this page.
    // Reasons for this are missing text or a vote which needs to be 
    //   processed still.
    if (!self::$colored_text || WikiTrust::voteToProcess($rev_id)){
      $modified_times['page'] = wfTimestampNow();
      wfWikiTrustDebug(__FILE__.":".__LINE__
                       .": new times - ".print_r($modified_times, true));
    }
    return true;
  }

  // TrustTabSkin - add trust tab to display, and select if appropriate
  public static function ucscTrustTemplate($skin, &$content_actions)
  {
    global $wgRequest;
    if (($wgRequest->getVal('action') 
         && ($wgRequest->getVal('action') != 'purge')) 
        || $wgRequest->getVal('diff')) {
      // we don't want trust for actions which are not purges or diffs.
      return true;
    }

    // Builds up the query string for when a user clicks on the show 
    // trust button. 
    $trust_qs = $_SERVER['QUERY_STRING'];
    wfWikiTrustDebug(__FILE__ . ":" . __LINE__ . ": Query String: $trust_qs");
    if ($trust_qs) {
      // If there is already something after the ? in the page url:
      if (!stristr($trust_qs, "trust")){
        // If there is not a trust=t, add it.
        $trust_qs = "?" . $trust_qs . "&trust";
      } else {
        // Otherwise, just add a ? back.
        $trust_qs = "?" . $trust_qs;
      }
    } else {
      // If there is nothing after the ?, add what we need.
      $trust_qs = "?trust"; 
    }
    
    wfLoadExtensionMessages('WikiTrust');
    $content_actions['trust'] = array (
				    'class' => '',
				    'text' => wfMsgNoTrans("wgTrustTabText"),
				    'href' => $_SERVER['PHP_SELF'] . $trust_qs
				);
    
    $use_trust = $wgRequest->getVal('trust'); 
    if (isset($use_trust)){
      $content_actions['trust']['class'] = 'selected';
      $content_actions['nstab-main']['class'] = '';
      $content_actions['nstab-main']['href'] .= '';
    } else {
      $content_actions['trust']['href'] .= '';
    }
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
      global $wgDBname, $wgDBuser, $wgDBpassword, $wgDBserver, $wgDBtype, $wgWikiTrustCmd, $wgWikiTrustLog, $wgWikiTrustDebugLog, $wgWikiTrustRepSpeed, $wgDBprefix, $wgWikiTrustCmdExtraArgs;
      
      $process = -1;
      $command = "";
      // Get the db.
      $dbr =& wfGetDB( DB_SLAVE );
	  
      // Do we use a DB prefix?
      $prefix = ($wgDBprefix)? "-db_prefix " . $dbr->strencode($wgDBprefix): "";
	  
      switch ($eval_type) {
	  case self::TRUST_EVAL_EDIT:
	      $command = escapeshellcmd("$wgWikiTrustCmd -rep_speed $wgWikiTrustRepSpeed -log_file $wgWikiTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname $prefix $wgWikiTrustCmdExtraArgs") . " &";
	      break;
	  case self::TRUST_EVAL_VOTE:
	      if ($rev_id == -1 || $page_id == -1 || $voter_id == -1)
		  return -1;
	      $command = escapeshellcmd("$wgWikiTrustCmd -eval_vote -rev_id " . $dbr->strencode($rev_id) . " -voter_id " . $dbr->strencode($voter_id) . " -page_id " . $dbr->strencode($page_id) . " -rep_speed $wgWikiTrustRepSpeed -log_file $wgWikiTrustLog -db_host $wgDBserver -db_user $wgDBuser -db_pass $wgDBpassword -db_name $wgDBname $prefix $wgWikiTrustCmdExtraArgs") . " &";
	      break;
      }
	  
      $descriptorspec = array(
	      0 => array("pipe", "r"),
	      1 => array("file", escapeshellcmd($wgWikiTrustDebugLog), "a"),
	      2 => array("file", escapeshellcmd($wgWikiTrustDebugLog), "a")
	  );

      wfWikiTrustDebug(__FILE__ . ":" . __LINE__ . ": $command");
      
      $cwd = '/tmp';
      $env = array();
      wfWikiTrustDebug(__FILE__.":".__LINE__ 
        . ": wikitrustbase.php calling " . $command);
      $process = proc_open($command, $descriptorspec, $pipes, $cwd, $env);

      return $process; 
  }


  static function user_GetIdFName(&$dbr, $userName)
  {
    if (preg_match("/^\d+\.\d+\.\d+\.\d+$/", $userName))
	return 0;		// IP addrs are anonymous

    $res = $dbr->select('user', array('user_id'), 
		    array('user_name' => $userName), array());
    if ($res && $dbr->numRows($res) > 0){
      $row = $dbr->fetchRow($res);
      $user_id = $row['user_id'];
      if (!$user_id)
	$user_id = 0;
    }
    $dbr->freeResult( $res ); 

    return $user_id;
  }
      
  /* Utility function which returns the revid of the revision which is 
   * currently being displayed.
   * It tries to do this using the global wgArticle object, and if this fails
   * queries the DB directly.
   *
   * This is a replacement for getRevFTitle() and eliminates 1 db call.
   */
  static function util_getRevFArticle()
  {
    // If no revid, assume it is the most recent one.
    // Try using the article object, and only if this fails use the Title.
    global $wgArticle;
    $rev_id = 0;
    if (method_exists($wgArticle, "getLatest"))
      $rev_id = $wgArticle->getLatest();
    else { // Otherwise, ask the db for the latest revid for the given pageid.
      global $wgTitle;
      $rev_id = 0;
      $page_id = $wgTitle->getArticleID();
      $dbr =& wfGetDB( DB_SLAVE );
      $res = $dbr->select('page', array('page_latest'), 
                          array('page_id' => $page_id), array());
      if ($res && $dbr->numRows($res) > 0) {
        $row = $dbr->fetchRow($res);
        $rev_id = $row['page_latest'];
      }
      $dbr->freeResult( $res ); 
    }
    return $rev_id;
  }

  // Returns the current revid from the $out object.
  static function util_getRevFOut($out)
  {
    if (method_exists($out, "getRevisionId"))
      $rev_id = $out->getRevisionId();
    else
      $rev_id = $out->mRevisionId;

    if (!$rev_id) {
      $rev_id = self::util_getRevFArticle();
    }

    return $rev_id;
  }

  // Likewise, this returns the current revid.
  // If oldid is set, it returns this.
  // Otherwise, it called util_getRevFArticle().
  static function util_getRev()
  {
    global $wgRequest;
    $rev_id = $wgRequest->getVal('oldid');
    if (!$rev_id) {
      $rev_id = self::util_getRevFArticle();
    }
    
    return $rev_id;
  }

  static function util_getRevFilename($page_id, $rev_id)
  {
    $page_str = sprintf("%012d", $page_id);
    $rev_str = sprintf("%012d", $rev_id);
    global $wgWikiTrustColorPath;
    $path = $wgWikiTrustColorPath;
    for ($i = 0; $i <= 3; $i++)
	$path .= "/" . substr($page_str, $i*3, 3);
    $path .= "/" . substr($rev_str, 6, 3);
    $path .= "/" . $page_str . "_" . $rev_str . ".gz";
    return $path;
  }

  static function debug($msg, $level)
  {
    global $wgWikiTrustDebugLog, $wgWikiTrustDebugVerbosity;

    if ($level >= $wgWikiTrustDebugVerbosity) 
      file_put_contents($wgWikiTrustDebugLog, 
                        $msg . PHP_EOL,
                        FILE_APPEND|LOCK_EX);
  }
}

?>
