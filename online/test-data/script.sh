#!/bin/bash

cat ../sql/reset_online_analysis.sql  | mysql --user="root" --password="octopus8legs" wikidb1
cd ../wikifeed
./set_test.py ../test-data/wiki_00023.xml
cd ../analysis
ocamldebug -I ../../batch/analysis -I ../../../OcamlLdaLibs -I ../../../OcamlLdaLibs/xml-light ./eval_online_wiki -db_user wikiuser -db_pass wikiword -db_name wikidb1 -log_name ~/temp/color.log
