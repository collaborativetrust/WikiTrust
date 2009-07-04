<?php

class WikiTrust extends WikiTrustBase {

    static function ucscOutputBeforeHTML(&$out, &$text) {
	color_addJsAndCss($out);
	super::ucscOutputBeforeHTML($out, $text);
	trustdata_loadFDb($out);
	return true;
    }

}

?>
