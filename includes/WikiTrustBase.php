<?php

class WikiTrustBase {
  ## Cache time that results are valid for.  Currently 1 day.
  const TRUST_CACHE_VALID = 31556926;

  ## Types of analysis to perform.
  const TRUST_EVAL_VOTE = 0;
  const TRUST_EVAL_EDIT = 10;
  const TRUST_EVAL_MISSING = 15;

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

  ## Server forms
  const NOT_FOUND_TEXT_TOKEN = "TEXT_NOT_FOUND";
  const TRUST_COLOR_TOKEN = "<!-- trust -->";

  ## Context for communicating with the trust server
  const TRUST_TIMEOUT = 10;

  ## Default median to avoid div by 0 errors
  const TRUST_DEFAULT_MEDIAN = 1;

  ## Median Value of Trust
  static $median = 1.0;

  ## Don't close the first opening span tag
  static $first_span = true;
    
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
			     $rev_id_raw = 0, $page_title_raw = ""){
    
    global $wgWikiTrustContentServerURL;
    $response = new AjaxResponse("0");
    
    $dbr =& wfGetDB( DB_SLAVE );
    
    $userName = $dbr->strencode($user_name_raw, $dbr);
    $page_id = $dbr->strencode($page_id_raw, $dbr);
    $rev_id = $dbr->strencode($rev_id_raw, $dbr);
    $page_title = $dbr->strencode($page_title_raw, $dbr);
    
    if($page_id){
      // First, look up the id numbers from the page and user strings
      $res = $dbr->select('user', array('user_id'), 
			array('user_name' => $userName), array());
      if ($res){
	$row = $dbr->fetchRow($res);
	$user_id = $row['user_id'];
	if (!$user_id) {
	  $user_id = 0;
	}
      }
      $dbr->freeResult( $res ); 

      $response = self::vote_recordVote($user_id, $page_id, $rev_id)
    }
    return $response;
  }

  static function vote_recordVote($user_id, $page_id, $rev_id) {
			// Now see if this user has not already voted, 
			// and count the vote if its the first time though.
      $res = $dbr->select('wikitrust_vote', array('revision_id'),
		array('revision_id' => $rev_id, 'voter_id' => $user_id),
		array());
      if ($res) {
	$row = $dbr->fetchRow($res);
	if(!$row['revision_id']){
	  $insert_vals = array("revision_id" => $rev_id,
				 "page_id" => $page_id ,
				 "voter_id" => $user_id,
				 "voted_on" => wfTimestampNow()
			 );
	  $dbw =& wfGetDB( DB_MASTER );
	  if ($dbw->insert( 'wikitrust_vote', $insert_vals)){
	    $dbw->commit();
	    $response = new AjaxResponse(implode  ( ",", $insert_vals));
	    self::runEvalEdit(self::TRUST_EVAL_VOTE, $rev_id, $page_id, $user_id); 
	  }
	} else {
	  $response = new AjaxResponse("Already Voted");
	}
	$dbr->freeResult( $res ); 	   
      }
    return $response;
  }
  
  /*
   Callback for parser function.
  */
  static function handleParserRe($matches){
    
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

	/**
   Returns colored markup.
	 
   @return colored markup.
  */
  static function ucscOutputBeforeHTML(&$out, &$text){
    global $wgScriptPath;
		
    // Load the i18n strings
    wfLoadExtensionMessages('WikiTrust');

    // Add the css and js
    $out->addScript("<script type=\"text/javascript\" src=\""
		.$wgScriptPath
		."/extensions/WikiTrust/js/trust.js\"></script>");
    $out->addScript("<link rel=\"stylesheet\" type=\"text/css\" href=\""
		.$wgScriptPath."/extensions/WikiTrust/css/trust.css\">"); 
    return true;
  }

  static function trustdata_loadFDb()
  {
    global $wgParser, $wgUser, $wgTitle,
		$wgWikiTrustShowVoteButton, $wgUseAjax;
    $dbr =& wfGetDB( DB_SLAVE );
    
    if (method_exists($out, "getRevisionId"))
	$rev_id = $out->getRevisionId();
    else
	$rev_id = $out->mRevisionId;
    $page_id = $wgTitle->getArticleID();
    $page_title = $wgTitle->getDBkey();
    $user_id = $wgUser->getID();

    // If there is not a revId, assume it is the most recent one.
    if(!$rev_id) {
      $res = $dbr->select('page', array('page_latest'), 
                          array('page_id' => $page_id), array());
      if ($res){
        $row = $dbr->fetchRow($res);
        $rev_id = $row['page_latest'];
      }
      $dbr->freeResult( $res ); 
    }
    
    // Set this up so we can parse things later.
    $options = ParserOptions::newFromUser($wgUser);
    $colored_text = "";

    $res = $dbr->select('wikitrust_colored_markup', 'revision_text',
			array( 'revision_id' => $rev_id ), 
			array());
    if ($res){
      $row = $dbr->fetchRow($res);
      $colored_text = $row[0];
    }
    $dbr->freeResult( $res ); 

    $res = $dbr->select('wikitrust_global', 'median',
			array(), 
			array());
    if ($res){
      $row = $dbr->fetchRow($res);
      self::$median = $row[0];
      if ($row[0] == 0){
	self::$median = self::TRUST_DEFAULT_MEDIAN;
      }
    }
    $dbr->freeResult( $res ); 

    if ($colored_text){
      // First, make sure that there are not any instances of our tokens in 
      // the colored_text
      $colored_text = str_replace(self::TRUST_OPEN_TOKEN, "", $colored_text);
      $colored_text = str_replace(self::TRUST_CLOSE_TOKEN, "", $colored_text);
      
      $colored_text = preg_replace("/&apos;/", "'", $colored_text, -1);      
      $colored_text = preg_replace("/&amp;/", "&", $colored_text, -1);
      $colored_text = preg_replace("/&lt;/", self::TRUST_OPEN_TOKEN, 
				 $colored_text, -1);
      $colored_text = preg_replace("/&gt;/", self::TRUST_CLOSE_TOKEN, 
				 $colored_text, -1);
			
      $parsed = $wgParser->parse($colored_text, $wgTitle, $options);
      $text = $parsed->getText();
      
      $count = 0;
      // Update the trust tags
      $text = preg_replace_callback("/\{\{#t:(\d+),(\d+),(.*?)\}\}/",
				"WikiTrust::handleParserRe",
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

      $text = '<script type="text/javascript" src="'
	      .$wgScriptPath
	      .'/extensions/WikiTrust/js/wz_tooltip.js"></script>' . $text;

      $msg = $wgParser->parse(wfMsgNoTrans("wgTrustExplanation"), 
				$wgTitle, 
				$options);
      $text = $text . $msg->getText();

      if ($wgWikiTrustShowVoteButton && $wgUseAjax){
	$text = "<div id='vote-button'><input type='button' name='vote' "
		. "value='" 
		. wfMsgNoTrans("wgTrustVote")
		. "' onclick='startVote()' /></div><div id='vote-button-done'>"
		. wfMsgNoTrans("wgTrustVoteDone") 
		. "</div>"
		. $text;
      }
    } else {
      // text not found.
      $msg = $wgParser->parse(wfMsgNoTrans("wgNoTrustExplanation"), 
				$wgTitle, 
				$options);
      $text = $msg->getText() . $text;
    }

    return true;
  }

	public static function ucscArticleSaveComplete(&$article, 
																								 &$user, 
																								 &$text, 
																								 &$summary,
																								 &$minoredit, 
																								 &$watchthis, 
																								 &$sectionanchor, 
																								 &$flags, 
																								 &$revision){
		
    $userName = $user->getName();
    $page_id = $article->getTitle()->getArticleID();
    $rev_id = $revision->getID();
		$page_title = $article->getTitle()->getDBkey();
		$user_id = $user->getID();
		$parentId = $revision->getParentId();
		
		if (self::runEvalEdit(self::TRUST_EVAL_EDIT, 
													$rev_id, 
													$page_id,
													$user_id) >= 0)
			return true;
		return false;
	}

  // TrustTabSkin - add trust tab to display, and select if appropriate
  public static function ucscTrustTemplate($skin, &$content_actions){
    global $wgRequest;
    wfLoadExtensionMessages('RemoteTrust');
    
    if ($wgRequest->getVal('action') || $wgRequest->getVal('diff')){
      // we don't want trust for actions or diffs.
      return true;
    }
    
    $trust_qs = $_SERVER['QUERY_STRING'];
    if($trust_qs){
      $trust_qs = "?" . $trust_qs .  "&trust=t";
    } else {
      $trust_qs .= "?trust=t"; 
    }
    
    $content_actions['trust'] = array ( 'class' => '',
					'text' => 
					wfMsgNoTrans("wgTrustTabText"),
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


}

?>
