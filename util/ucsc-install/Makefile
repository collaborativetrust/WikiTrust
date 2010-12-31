
WWW_DIR=/var/www/WikiTrust
WIKITRUST_DIR=/home/wikitrust

TOP:=../..

.PHONY: install www api munin dispatcher

install: www munin api dispatcher


www:
	mkdir -p $(WWW_DIR)
	cp -ar $(TOP)/js $(WWW_DIR)/
	cp -ar $(TOP)/css $(WWW_DIR)/

munin:
	mkdir -p $(WIKITRUST_DIR)/bin
	cp -a $(TOP)/util/munin/wikitrust_ $(WIKITRUST_DIR)/bin/

api:
	mkdir -p $(WIKITRUST_DIR)/perl
	cp -a $(TOP)/remote/analysis/WikiTrust.pm $(WIKITRUST_DIR)/perl/
	/etc/init.d/apache2 restart

dispatcher:
	mkdir -p $(WIKITRUST_DIR)/bin
	mv $(WIKITRUST_DIR)/bin/dispatcher $(WIKITRUST_DIR)/bin/dispatcher.old
	cp -a $(TOP)/remote/analysis/dispatcher $(WIKITRUST_DIR)/bin/
	cp -a $(TOP)/remote/admin/wikitrust $(WIKITRUST_DIR)/bin/
	cp -a $(TOP)/remote/admin/wikitrustd $(WIKITRUST_DIR)/bin/
	cp -a $(TOP)/remote/analysis/GetWikiFeed.php $(WIKITRUST_DIR)/bin/
	mkdir -p $(WIKITRUST_DIR)/WikiTrust/remote/analysis
	cp -a $(TOP)/remote/analysis/read_rev_text $(WIKITRUST_DIR)/WikiTrust/remote/analysis/
	ln -s -f $(WIKITRUST_DIR)/bin/wikitrust /etc/init.d/
	update-rc.d wikitrust defaults

