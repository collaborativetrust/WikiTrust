<?php
/*
 * ExtensionClass.php
 * 
 * MediaWiki extension
 * @author: Jean-Lou Dupont (http://www.bluecortex.com)
 * $Id$
 *
 * Purpose:  Provides a toolkit for easier Mediawiki
 *           extension development.
 *
 * FEATURES:
 * - 'singleton' implementation suited for extensions that require single instance
 * - 'magic word' helper functionality
 * - limited pollution of global namespace
 * - Automatic registration of hooks
 *
 * Tested Compatibility: MW 1.8.2 (PHP5), 1.9.3, 1.10
 *
 * History:
 * v1.0		Initial availability
 * v1.01    Small enhancement in processArgList
 * v1.02    Corrected minor bug
 * v1.1     Added function 'checkPageEditRestriction'
 * v1.2     Added 'getArticle' function
 * ----     Moved to SVN management
 * v1.3     Added wgExtensionCredits updating upon Special:Version viewing
 * v1.4     Fixed broken singleton functionality
 * v1.5		Added automatic registration of hook functions based
 *          on the definition of an handler in the derived class
 *          (e.g. if handler 'hArticleSave' exists, then the appropriate
 *           'ArticleSave' hook is registered)
 * v1.51    Fixed '$passingStyle' bug (thanks to Joshua C. Lerner)
 * v1.6     Added 'updateCreditsDescription' helper method.
 * v1.7		Added 'depth' parameter support: more than 1 class depth can be created.
 *          Added 'setupTags' method (support for parser tags)
 *          Enhancement to 'getParam' method
 *          Added 'formatParams' method
 * v1.8     Added 'initFirst' parameter
 * v1.9     Added support for including 'head' scripts and stylesheeets
 *          in a manner compatible with parser caching functionality.
 *          (Original idea from [user:Jimbojw]
 * v1.91    Added check for screening script duplicates in 'addHeadScript'
 * v1.92    Added optional removal of parameters not listed in template.
 * v1.93    Added 'replaceHook' method.
 *          (dependancy on 'replaceHookList')
 *
 */
$wgExtensionCredits['other'][] = array( 
	'name'    => 'ExtensionClass',
	'version' => 'v1.93 $Id$',
	'author'  => 'Jean-Lou Dupont', 
	'url'     => 'http://www.bluecortex.com',
);

class ExtensionClass
{
	static $gObj; // singleton instance

// List up-to-date with MW 1.10 SVN 21828
static $hookList = array(
'ArticlePageDataBefore', 
'ArticlePageDataAfter', 
'ArticleAfterFetchContent',
'ArticleViewRedirect', 
'ArticleViewHeader',
'ArticlePurge',
'ArticleSave', 
'ArticleInsertComplete',
'ArticleSaveComplete',
'MarkPatrolled', 
'MarkPatrolledComplete', 
'WatchArticle', 
'WatchArticleComplete',
'UnwatchArticle', 
'UnwatchArticleComplete', 
'ArticleProtect', 
'ArticleProtectComplete',
'ArticleDelete', 
'ArticleDeleteComplete', 
'ArticleEditUpdatesDeleteFromRecentchanges',
'ArticleEditUpdateNewTalk',
'DisplayOldSubtitle',
'IsFileCacheable',
'CategoryPageView',
'FetchChangesList',
'DiffViewHeader',
'AlternateEdit', 
'EditFormPreloadText', 
'EditPage::attemptSave', 
'EditFilter', 
'EditPage::showEditForm:initial',
'EditPage::showEditForm:fields',
'SiteNoticeBefore',
'SiteNoticeAfter',
'FileUpload',
'BadImage', 
'MagicWordMagicWords', 
'MagicWordwgVariableIDs',
'MathAfterTexvc',
'MessagesPreLoad',
'LoadAllMessages',
'OutputPageParserOutput',
'OutputPageBeforeHTML',
'AjaxAddScript', 
'PageHistoryBeforeList',
'PageHistoryLineEnding',
'ParserClearState', 
'ParserBeforeStrip',
'ParserAfterStrip',
'ParserBeforeTidy',
'ParserAfterTidy',
'ParserBeforeStrip',
'ParserAfterStrip', 
'ParserBeforeStrip',
'ParserAfterStrip', 
'ParserBeforeInternalParse',
'InternalParseBeforeLinks', 
'ParserGetVariableValueVarCache',
'ParserGetVariableValueTs', 
'ParserGetVariableValueSwitch',
'IsTrustedProxy',
'wgQueryPages', 
'RawPageViewBeforeOutput', 
'RecentChange_save',
'SearchUpdate', 
'AuthPluginSetup', 
'LogPageValidTypes',
'LogPageLogName', 
'LogPageLogHeader', 
'LogPageActionText',
'SkinTemplateTabs', 
'BeforePageDisplay', 
'SkinTemplateOutputPageBeforeExec', 
'PersonalUrls', 
'SkinTemplatePreventOtherActiveTabs',
'SkinTemplateTabs', 
'SkinTemplateBuildContentActionUrlsAfterSpecialPage',
'SkinTemplateContentActions', 
'SkinTemplateBuildNavUrlsNav_urlsAfterPermalink',
'SkinTemplateSetupPageCss',
'BlockIp', 
'BlockIpComplete', 
'BookInformation', 
'SpecialContributionsBeforeMainOutput',
'EmailUser', 
'EmailUserComplete',
'SpecialMovepageAfterMove',
'SpecialMovepageAfterMove',
'SpecialPage_initList',
'SpecialPageExecuteBeforeHeader',
'SpecialPageExecuteBeforePage',
'SpecialPageExecuteAfterPage',
'PreferencesUserInformationPanel',
'SpecialSearchNogomatch',
'ArticleUndelete',
'UndeleteShowRevision',
'UploadForm:BeforeProcessing',
'UploadVerification',
'UploadComplete',
'UploadForm:initial',
'AddNewAccount',
'AbortNewAccount',
'UserLoginComplete',
'UserCreateForm',
'UserLoginForm',
'UserLogout',
'UserLogoutComplete',
'UserRights',
/*'SpecialVersionExtensionTypes',*/ // reserved special treatment
'UnwatchArticle',
'AutoAuthenticate', 
'GetFullURL',
'GetLocalURL',
'GetInternalURL',
'userCan',
'TitleMoveComplete',
'isValidPassword',
'UserToggles',
'GetBlockedStatus',
'PingLimiter',
'UserRetrieveNewTalks',
'UserClearNewTalkNotification',
'PageRenderingHash',
'EmailConfirmed',
'ArticleFromTitle',
'CustomEditor',
'UnknownAction',
/*'LanguageGetMagic', */ // reserved a special treatment in this class 
'LangugeGetSpecialPageAliases',
'MonoBookTemplateToolboxEnd',
'SkinTemplateSetupPageCss',
'SkinTemplatePreventOtherActiveTabs'
);

	var $className;
	
	var $paramPassingStyle;
	var $ext_mgwords;	
	
	// Parameter passing style.
	const mw_style = 1;
	const tk_style = 2;
	
	public static function &singleton( $mwlist=null ,$globalObjName=null, 
										$passingStyle = self::mw_style, $depth = 1,
										$initFirst = false )
	{
		// Let's first extract the callee's classname
		$trace = debug_backtrace();
		$cname = $trace[$depth]['class'];

		// If no globalObjName was given, create a unique one.
		if ($globalObjName === null)
			$globalObjName = substr(create_function('',''), 1 );
		
		// Since there can only be one extension with a given child class name,
		// Let's store the $globalObjName in a static array.
		if (!isset(self::$gObj[$cname]) )
			self::$gObj[$cname] = $globalObjName; 
				
		if ( !isset( $GLOBALS[self::$gObj[$cname]] ) )
			$GLOBALS[self::$gObj[$cname]] = new $cname( $mwlist, $passingStyle, $depth, $initFirst );
			
		return $GLOBALS[self::$gObj[$cname]];
	}
	public function ExtensionClass( $mgwords=null, $passingStyle = self::mw_style, 
									$depth = 1, $initFirst = false, $replaceHookList = null )
	/*
	 *  $mgwords: array of 'magic words' to subscribe to *if* required.
	 */
	{
		global $wgHooks;
			
		if ($passingStyle == null) $passingStyle = self::mw_style; // prevention...
		$this->paramPassingStyle = $passingStyle;
		
		// Let's first extract the callee's classname
		$trace = debug_backtrace();
		$this->className= $cname = $trace[$depth]['class'];
		// And let's retrieve the global object's name
		$n = self::$gObj[$cname];
		
		global $wgExtensionFunctions;
		
		// v1.8 feature
		$initFnc = create_function('',"global $".$n."; $".$n."->setup();");
		if ($initFirst)
			 array_unshift(	$wgExtensionFunctions, $initFnc );
		else $wgExtensionFunctions[] = $initFnc;
		
		$this->ext_mgwords = $mgwords;		
		if (is_array($this->ext_mgwords) )
			$wgHooks['LanguageGetMagic'][] = array($this, 'getMagic');

		// v1.3 feature
		if ( in_array( 'hUpdateExtensionCredits', get_class_methods($this->className) ) )
			$wgHooks['SpecialVersionExtensionTypes'][] = array( &$this, 'hUpdateExtensionCredits' );				

		// v1.5 feature
		foreach (self::$hookList as $index => $hookName)
		{
			if (!empty($replaceHookList))
				$replaceFlag = in_array( $hookName, $replaceHookList);
					
			if ( in_array( 'h'.$hookName, get_class_methods($this->className) ) )
			{
				if ( $replaceFlag )
					$wgHooks[$hookName][count($wgHooks[$hookName])-1] = array( &$this, 'h'.$hookName );
				else
					$wgHooks[$hookName][] = array( &$this, 'h'.$hookName );
			}
		}
	}
	public function getParamPassingStyle() { return $this->passingStyle; }
	public function setup()
	{
		if (is_array($this->ext_mgwords))
			$this->setupMagic();
	}
	// ================== MAGIC WORD HELPER FUNCTIONS ===========================
	public function getMagic( &$magicwords, $langCode )
	{
		foreach($this->ext_mgwords as $index => $key)
			$magicwords [$key] = array( 0, $key );
		return true;
	}
	public function setupMagic( )
	{
		global $wgParser;
		foreach($this->ext_mgwords as $index => $key)
			$wgParser->setFunctionHook( "$key", array( $this, "mg_$key" ) );
	}
	public function setupTags( $tagList )
	{
		global $wgParser;
		foreach($tagList as $index => $key)
			$wgParser->setHook( "$key", array( $this, "tag_$key" ) );
	}
	// ================== GENERAL PURPOSE HELPER FUNCTIONS ===========================
	public function processArgList( $list, $getridoffirstparam=false )
	/*
	 * The resulting list contains:
	 * - The parameters extracted by 'key=value' whereby (key => value) entries in the list
	 * - The parameters extracted by 'index' whereby ( index = > value) entries in the list
	 */
	{
		if ($getridoffirstparam)   
			array_shift( $list );
			
		// the parser sometimes includes a boggie
		// null parameter. get rid of it.
		if (count($list) >0 )
			if (empty( $list[count($list)-1] ))
				unset( $list[count($list)-1] );
		
		$result = array();
		foreach ($list as $index => $el )
		{
			$t = explode("=", $el);
			if (!isset($t[1])) 
				continue;
			$result[ "{$t[0]}" ] = $t[1];
			unset( $list[$index] );
		}
		if (empty($result)) 
			return $list;
		return array_merge( $result, $list );	
	}
	public function getParam( &$alist, $key, $index, $default )
	/*
	 *  Gets a parameter by 'key' if present
	 *  or fallback on getting the value by 'index' and
	 *  ultimately fallback on default if both previous attempts fail.
	 */
	{
		if (array_key_exists($key, $alist) )
			return $alist[$key];
		elseif (array_key_exists($index, $alist) && $index!==null )
			return $alist[$index];
		else
			return $default;
	}
	public function initParams( &$alist, &$templateElements, $removeNotInTemplate = true )
	{
			#var_dump( $alist );
		// v1.92 feature.
		if ($removeNotInTemplate)
			foreach( $templateElements as $index => &$el )
				if ( !isset($alist[ $el['key'] ]) )
					unset( $alist[$el['key']] );
		
		foreach( $templateElements as $index => &$el )
			$alist[$el['key']] = $this->getParam( $alist, $el['key'], $el['index'], $el['default'] );
	}
	public function formatParams( &$alist , &$template )
	// look at yuiPanel extension for usage example.
	// $alist = { 'key' => 'value' ... }
	{
		foreach ( $alist as $key => $value )
			// format the entry.
			$this->formatParam( $key, $value, $template );
	}
	private function formatParam( &$key, &$value, &$template )
	{
		$format = $this->getFormat( $key, $template );
		if ($format !==null )
		{
			switch ($format)
			{
				case 'bool':   $value = (bool) $value; break; 
				case 'int':    $value = (int) $value; break;
				default:
				case 'string': $value = (string) $value; break;					
			}			
		}
	}
	public function getFormat( &$key, &$template )
	{
		$format = null;
		foreach( $template as $index => &$el )
			if ( $el['key'] == $key )
				$format  = $el['format'];
			
		return $format;
	}
	public function checkPageEditRestriction( &$title )
	// v1.1 feature
	// where $title is a Mediawiki Title class object instance
	{
		$proceed = false;
  
		$state = $title->getRestrictions('edit');
		foreach ($state as $index => $group )
			if ( $group == 'sysop' )
				$proceed = true;

		return $proceed;		
	} 
	public function getArticle( $article_title )
	{
		$title = Title::newFromText( $article_title );
		  
		// Can't load page if title is invalid.
		if ($title == null)	return null;
		$article = new Article($title);

		return $article;	
	}
	
	function isSysop( $user = null ) // v1.5 feature
	{
		if ($user == null)
		{
			global $wgUser;
			$user = $wgUser;
		}	
		return in_array( 'sysop', $user->getGroups() );
	}
	
	function updateCreditsDescription( &$text ) // v1.6 feature.
	{
		global $wgExtensionCredits;
	
		foreach ( $wgExtensionCredits[self::thisType] as $index => &$el )
			if ($el['name']==self::thisName)
				$el['description'].=$text;	
	}

/*  Add scripts & stylesheets functionality.
This process must be done in two phases:
phase 1- encode information related to the required
         scripts & stylesheets in a 'meta form' in
		 the parser cache text.
phase 2- when the page is rendered, extract the meta information
         and include the information appropriately in the 'head' of the page.		  
************************************************************************************/
	static $scriptList;
	static $scriptsAdded;
	static $scriptsListed;

	function addHeadScript( $st )
	{
		if ( !isset($st) ) return;
		
		if ( !isset(self::$scriptList) )
			self::$scriptList[] = $st;
		elseif	(!in_array($st, self::$scriptList)) 
			self::$scriptList[] = $st;
			 
		self::$scriptsAdded = false;
		self::$scriptsListed = false;
	}
	
	function hParserAfterTidy( &$parser, &$text )
	// set the meta information in the parsed 'wikitext'.
	{
		if (self::$scriptsListed) return true;
		self::$scriptsListed = true;

		if (!empty(self::$scriptList))
			foreach(self::$scriptList as $sc)
				$text .= '<!-- META_KEYWORDS '.base64_encode($sc).' -->'; 

		return true;
	}	
	function hOutputPageBeforeHTML( &$op, &$text )
	// This function sifts through 'meta tags' embedded in html comments
	// and picks out scripts & stylesheet references that need to be put
	// in the page's HEAD.
	{
		// some hooks get called more than once...
		// In this case, since ExtensionClass provides a 
		// base class for numerous extensions, then it is very
		// likely this method will be called more than once;
		// so, we want to make sure we include the head scripts just once.
		if (self::$scriptsAdded) return true;
		self::$scriptsAdded = true;
		
		if (preg_match_all(
        	'/<!-- META_KEYWORDS ([0-9a-zA-Z\\+\\/]+=*) -->/m', 
        	$text, 
        	$matches)===false) return true;
			
    	$data = $matches[1];

	    foreach ($data AS $item) 
		{
	        $content = @base64_decode($item);
	        if ($content) $op->addScript( $content );
	    }
	    return true;
	}

} // end class definition.
?>