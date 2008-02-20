#!/bin/bash

# Execute this as root

# Replace with what you need
media_dir='/opt/local/apache2/htdocs/mediawiki-1.10.1'

# Add our extensions
cp -r extensions/Trust ${media_dir}/extensions/
cp skins/Trust.php ${media_dir}/skins/
cp -r skins/trust ${media_dir}/skins/

# For the trust extension
# IMPORTANT: you need to install the "tidy" extension
cat >> ${media_dir}/LocalSettings.php <<EOF
require_once( "extensions/Trust/Trust3.php" );
$wgUseTidy = true;
require_once("$IP/extensions/ParserFunctions/ParserFunctions.php");
require_once("$IP/extensions/Cite/Cite.php");
$wgDefaultSkin = 'trust';
EOF

# Now make things readable
chmod -R a+rX ${media_dir}



