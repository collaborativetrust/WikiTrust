<?php

/**
  Manages the file cache for mediawiki -- text is stored on the filesystem instead of in a database
  This makes for easer loading and dumping of the DB, as well as being a LOT easier to manage.

  2007 - Ian Pye - <ipye@cs.ucsc.edu>
  Trust in Wikipedia Project, UCSC
  trust.cse.ucsc.edu
*/

class TextFileCache{
 
  private $data;
  private $rev_id;
  private $page_id;

  function __construct( ) {
  }

  /**
   * Stores the data in the fs cache
   */
  public function setTextInCache( $page_id, $data ){

    // pad the page and revision strings to the proper length                   
    $page_id = str_pad ($page_id, FS_CACHE_PAD_LENGTH, "0", STR_PAD_LEFT);

    $this->data = $data;
    $this->page_id = $page_id;

    return ""; // this text will be inserted into the db 
  }

  public function commitTextInsert( $rev_id ){
    $rev_id = str_pad ($rev_id, FS_CACHE_PAD_LENGTH, "0", STR_PAD_LEFT);
  
    $page_path = TextFileCache::getPagePath( $this->page_id, true );

    $status = file_put_contents( $page_path . LOCAL_FILE_SYSTEM_SEP . $rev_id
                                            . FS_CACHE_REV_EXTENSION, $this->data );
    if (FS_CACHE_DEBUG){
      if( !$status ){
        file_put_contents (FS_CACHE_LOG, "PUT FAILURE: ".$page_path 
            . LOCAL_FILE_SYSTEM_SEP . $rev_id
            . FS_CACHE_REV_EXTENSION . "\n", FILE_APPEND );
      } else {
        file_put_contents (FS_CACHE_LOG, "PUT: ".$page_path 
            . LOCAL_FILE_SYSTEM_SEP . $rev_id
            . FS_CACHE_REV_EXTENSION . "\n", FILE_APPEND );
      }
    }
    return $status;
  }


//****** Only Static Functions Below **********//  

  /**
   * strips coloring tags out of tables, where they should not be
   */
  public static function c_stripTables( $matches ){
    $text = preg_replace("/{{#t:\d}}/", "", $matches[2]);
    $text = preg_replace("/{{to:\d*?}}/", "", $text);
    #return "";
    return $matches[1].$text;
  } 
  
  /**
   * Given a page id, return the path to the directory for the page
   * Also, optionally create the directory
   *
   * @param string $page_id 
   * @param bool $create_dirs : default false
   */
  private static function getPagePath( $page_id, $create_dirs = false ){
    $page_dirs = str_split($page_id, FS_CACHE_PAGE_DIR_LENGTH);                           
    $page_path = FS_CACHE_LOCAL_SITE;                                                     
    foreach ($page_dirs as &$dir) {                                                       
      $page_path .= LOCAL_FILE_SYSTEM_SEP . $dir;                                         
    }

    if ($create_dirs){
      if(!file_exists($page_path)){
        mkdir ($page_path, FS_CACHE_DIR_CREATE_MODE, true );
      }
    }
    return $page_path;
  }

  /**
   * Loads the text with the given id from the filesystem in to memory and
   *  returns a string
   *
   * @param int $id
   * @access public
   * @static
   */
  public static function getTextFromCache( $page_id, $rev_id ){

    // pad the page and revision strings to the proper length
    $page_id = str_pad ($page_id, FS_CACHE_PAD_LENGTH, "0", STR_PAD_LEFT); 
    $rev_id = str_pad ($rev_id, FS_CACHE_PAD_LENGTH, "0", STR_PAD_LEFT); 

    $page_path = TextFileCache::getPagePath( $page_id );

    $text = html_entity_decode( ltrim ( file_get_contents( $page_path . LOCAL_FILE_SYSTEM_SEP . $rev_id 
        . FS_CACHE_REV_EXTENSION ) ) );

    // strip tags that should not be there
    if (FS_CACHE_FIX_REDIRECTS){
      $text = preg_replace( "/^({{#t:\d}}#|#{{#t:\d}}|#{{to:\d*?}}|#{{#t:\d}}{{to:\d*?}})redirect/i", "#REDIRECT", $text);
    }
    
    // fix tables...                                                                         
    if (FS_CACHE_FIX_TABLES){                                                                
      $text = preg_replace_callback (                                                        
                                   "/({\|||-)(.*)}/",                                                                  
                                   array('TextFileCache', 'c_stripTables'),                                           
                                   $text                                                                              
      );                                                                                     
    }   
    if( !$text ){
      if (FS_CACHE_DEBUG){
        file_put_contents (FS_CACHE_LOG, "GET FAILURE: ".$page_path 
            . LOCAL_FILE_SYSTEM_SEP . $rev_id 
            . FS_CACHE_REV_EXTENSION . "\n", FILE_APPEND );
      }
      return "";
    }
    if (FS_CACHE_DEBUG){
      file_put_contents (FS_CACHE_LOG, "GOT: ".$page_path
          . LOCAL_FILE_SYSTEM_SEP . $rev_id 
          . FS_CACHE_REV_EXTENSION . "\n", FILE_APPEND );
    }
    return $text;
  }
}
?>
