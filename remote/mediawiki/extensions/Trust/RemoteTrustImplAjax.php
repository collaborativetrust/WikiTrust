<?php

class TextTrustImpl {

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
  const TRUST_COLOR_TOKEN = "<!--trust-->";

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
    
    global $wgContentServerURL;
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
      
      $ctx = stream_context_create(
				   array('http' => array(
							 'timeout' => 
							 self::TRUST_TIMEOUT
							 )
					 )
				   );
      
      $vote_str = ("Voting at " . $wgContentServerURL . "vote=1&rev=$rev_id&page=$page_id&user=$user_id&page_title=$page_title&time=" . wfTimestampNow());
      $colored_text = file_get_contents($wgContentServerURL . "vote=1&rev=".urlencode($rev_id)."&page=".urlencode($page_id)."&user=".urlencode($user_id)."&page_title=".urlencode($page_title)."&time=" . urlencode(wfTimestampNow()), 0, $ctx);
      $response = new AjaxResponse($vote_str);	   
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
      . "onmouseover=\"Tip('".$matches[3]."')\" onmouseout=\"UnTip()\""
      . "onclick=\"showOrigin(" 
      . $matches[2] . ")\"" . self::TRUST_CLOSE_TOKEN;
    
    if (self::$first_span){
      self::$first_span = false;
    } else {
      $output = self::TRUST_OPEN_TOKEN . "/span" . self::TRUST_CLOSE_TOKEN . $output;
    }
    return $output;
  }

  /*
   Callback for Images.
  */
  static function getImageInfo($matches){
    global $wgWikiApiURL;

    $data = array('action'=>'query',
		  'prop'=>'imageinfo',
		  'titles'=>"File:".$matches[1],
		  'iiprop'=>'url|size',
		  'format' => 'json'
		  );
    
    // Don't do this now.
    /**
    $image_info_raw = file_get_contents($wgWikiApiURL
				       .http_build_query($data));
    $image_json = json_decode($image_info_raw, true);
    $page_ids_arr = array_keys($image_json["query"]["pages"]);
    $page_id = $page_ids_arr[0];
    $width = $image_json["query"]["pages"][$page_id]["imageinfo"][0]["width"];
    $height = $image_json["query"]["pages"][$page_id]["imageinfo"][0]["height"];
    $url = $image_json["query"]["pages"][$page_id]["imageinfo"][0]["url"];

    return  '<a href="/wiki/File:'.$matches[1].'" class="image"><img alt="" src="'.$url.'" width="'.$width.'" height="'.$height.'" border="0" class="thumbimage" /></a>';

    */
    return '<a href="/wiki/File:'.$matches[1].'" class="image">File:'.$matches[1].'</a>';
  }

  /**
   Method to use the wiki API to parse the markup.
   This allows templates to be resolved, as well as images and other good 
   things.
  */
  static function parseWikiText($text){
    global $wgWikiApiURL;

    $data = array('action'=>'parse',
		  'text'=>htmlspecialchars($text),
		  'format' => 'json'
		  );

     file_put_contents("/tmp/test", $wgWikiApiURL
				    .http_build_query($data));
    
    $parsed_raw = file_get_contents($wgWikiApiURL
				    .http_build_query($data));
    $parsed_json = json_decode($parsed_raw, true);
    $parsed_text = $parsed_json["parse"]["text"];
    return $parsed_text;
  }

   /**
   Returns colored markup.

   @return colored markup.
  */
  static function getColoredText($page_title_raw,
				 $page_id_raw = NULL, 
				 $rev_id_raw = NULL){
    global $wgParser, $wgContentServerURL, $wgWikiApiURL, $wgUser;
    $response = new AjaxResponse("0");
    
    if(!$page_id_raw || !$rev_id_raw){
      $data = array('action'=>'query',
		    'prop'=>'revisions',
		    'titles'=>$page_title_raw,
		    'rvlimit'=>'1',
		    'rvprop' => 'ids',
		    'format' => 'json'
		    );
      
      $page_info_raw = file_get_contents($wgWikiApiURL
					 .http_build_query($data));
      $page_json = json_decode($page_info_raw, true);
      $pages_arr = array_keys($page_json["query"]["pages"]);

      // Now, parse out only what we need
      if(!$page_id_raw){
	$page_id_raw = $pages_arr[0];
      }

      if(!$rev_id_raw){
	$rev_id_raw = $page_json["query"]["pages"][$page_id_raw]["revisions"][0]["revid"];
      }
    }

    $dbr =& wfGetDB( DB_SLAVE );
    
    $page_id = $dbr->strencode($page_id_raw, $dbr);
    $rev_id = $dbr->strencode($rev_id_raw, $dbr);
    $page_title = $dbr->strencode($page_title_raw, $dbr);
    
    $ctx = stream_context_create(
				 array('http' => array(
						       'timeout' => 
						       self::TRUST_TIMEOUT
						       )
				       )
				 );
    
    // Should we do doing this via HTTPS?
    $colored_raw = (file_get_contents($wgContentServerURL . "rev=" . 
				      urlencode($rev_id) . 
				      "&page=".urlencode($page_id).
				      "&page_title=".
				      urlencode($page_title)."&time=".
				      urlencode(wfTimestampNow())."&user="
				      .urlencode(0)."", 0, $ctx));
    
    if ($colored_raw && $colored_raw != self::NOT_FOUND_TEXT_TOKEN){
    
      // Inflate. Pick off the first 10 bytes for python-php conversion.
      $colored_raw = gzinflate(substr($colored_raw, 10));
      
      // Pick off the median value first.
      $colored_data = explode(",", $colored_raw, 2);
      $colored_text = $colored_data[1];
      if (preg_match("/^[+-]?(([0-9]+)|([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)|
			    (([0-9]+|([0-9]*\.[0-9]+|[0-9]+\.[0-9]*))[eE][+-]?[0-9]+))$/", $colored_data[0])){
	self::$median = $colored_data[0];
	if ($colored_data[0] == 0){
	  self::$median = self::TRUST_DEFAULT_MEDIAN;
	}
      }

      // First, make sure that there are not any instances of our tokens in the colored_text
      $colored_text = str_replace(self::TRUST_OPEN_TOKEN, "", $colored_text);
      $colored_text = str_replace(self::TRUST_CLOSE_TOKEN, "", 
				  $colored_text);
      
      $colored_text = preg_replace("/&apos;/", "'", $colored_text, -1);
      
      $colored_text = preg_replace("/&amp;/", "&", $colored_text, -1);
      
      $colored_text = preg_replace("/&lt;/", self::TRUST_OPEN_TOKEN, 
				   $colored_text, -1);
      $colored_text = preg_replace("/&gt;/", self::TRUST_CLOSE_TOKEN, 
				   $colored_text, -1);

      $title = Title::newFromText($page_title);
      $options = ParserOptions::newFromUser($wgUser);
      $parsed = $wgParser->parse($colored_text, $title, $options);
      $text = $parsed->getText();
      
      $count = 0;
      $text = preg_replace_callback("/\{\{#t:(\d+),(\d+),(.*?)\}\}/",
				    "TextTrustImpl::handleParserRe",
				    $text,
				    -1,
				    $count
				    );
      // $text = '<script type="text/javascript" src="http://redherring.cse.ucsc.edu/firefox/frontend/extensions/Trust/js/wz_tooltip.js"></script>' . $text;
      $text = preg_replace('/' . self::TRUST_OPEN_TOKEN . '/', "<", $text, -1, $count);
      $text = preg_replace('/<a href="(.*?)Image:(.*?)" (.*?)>/', self::TRUST_OPEN_TOKEN, $text, -1, $count);
      $text = preg_replace('/<a href="(.*?)title=(.*?)&amp;action=edit&amp;redlink=1" class="new" title="(.*?) \(not yet written\)">/', '<a href="/wiki/$2" title="$3">', $text, -1, $count);
      $text = preg_replace_callback(
				    '/'.self::TRUST_OPEN_TOKEN.'Image:(.*?)<\/a>/'
				    ,"TextTrustImpl::getImageInfo"
				    ,$text, -1, $count);
      
      $text = preg_replace('/' . self::TRUST_CLOSE_TOKEN .'/', ">", $text, -1, $count);
      $text = preg_replace('/<\/p>/', "</span></p>", $text, -1, $count);
      $text = preg_replace('/<p><\/span>/', "<p>", $text, -1, $count);
      $text = preg_replace('/<li><\/span>/', "<li>", $text, -1, $count);
      $response = new AjaxResponse($text);
    } else {
      // text not found.
      $response = new AjaxResponse(self::NOT_FOUND_TEXT_TOKEN);
    } 
    
    return $response;
  }
}

?>
