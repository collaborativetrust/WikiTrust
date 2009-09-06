# Copyright (c) 2007-2009 The Regents of the University of California
# All rights reserved.
#
# Authors: Luca de Alfaro, B. Thomas Adler, Ian Pye
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# 3. The names of the contributors may not be used to endorse or promote
# products derived from this software without specific prior written
# permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

# Vars we use in our rules to build ocaml programs
PACKAGES = unix,str,vec,mapmin,hashtbl_bounded,fileinfo,intvmap,extlib,mysql,sexplib.syntax,camlzip
SYNTAX = camlp4o
OUR_LIBS = evalwiki.cma
OUR_LIBS_OPT = evalwiki.cmxa
INCLUDES = 

OCAMLC=ocamlfind ocamlc -dtypes -package $(PACKAGES) -syntax $(SYNTAX)
OCAMLOPT=ocamlfind ocamlopt -package $(PACKAGES) -syntax $(SYNTAX)

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES) -syntax $(SYNTAX)
OCAMLDOC=ocamldoc

SUFFIXES= .ml .cmo .cmi .cmx

# For profiling
# OCAMLDEBUG = -g -p film 
OCAMLDEBUG = -g


# Flags for commands, depending on optimization/debugging
OCAML_CFLAGS=$(INCLUDES) $(OCAMLDEBUG)
OCAMLOPT_FLAGS=$(INCLUDES)

%.cmo: %.ml
	@echo '$(OCAMLC) $(OCAML_CFLAGS) -c $<'; \
	$(OCAMLC) $(OCAML_CFLAGS) -c $<

%.cmi: %.mli
	@echo '$(OCAMLC) $(OCAML_CFLAGS) -c $<'; \
	$(OCAMLC) $(OCAML_CFLAGS) -c $<

%.cmx: %.ml
	@echo '$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<'; \
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

.PHONY: all allopt universe clean

# Evalwiki does the batch evaluation

OUR_EVALWIKI_OBJS = textbuf.cmo text.cmo prioq.cmo filesystem_store.cmo \
	coda.cmo editlist.cmo chdiff.cmo compute_edlist.cmo timeconv.cmo \
	read_robots.cmo eval_defs.cmo author_sig.cmo online_types.cmo \
	revision_store.cmo rephist.cmo revision.cmo prune_analysis.cmo \
	linear_analysis.cmo circbuf_analysis.cmo reputation_analysis.cmo \
	compute_trust.cmo compute_robust_trust.cmo trust_analysis.cmo \
	trust_local_color_analysis.cmo author_text_analysis.cmo \
	contribution_analysis.cmo revcount_analysis.cmo \
	intertime_analysis.cmo \
	trust_origin_analysis.cmo revs_to_files_analysis.cmo \
	trust_for_online_analysis.cmo word_frequency.cmo \
	page_factory.cmo do_eval.cmo evalwiki.cmo

OUR_OPT_EVALWIKI_OBJS = textbuf.cmx text.cmx prioq.cmx filesystem_store.cmx \
	coda.cmx editlist.cmx chdiff.cmx compute_edlist.cmx timeconv.cmx \
	read_robots.cmx eval_defs.cmx author_sig.cmx online_types.cmx \
	revision_store.cmx rephist.cmx revision.cmx prune_analysis.cmx \
	linear_analysis.cmx circbuf_analysis.cmx reputation_analysis.cmx \
	compute_trust.cmx compute_robust_trust.cmx trust_analysis.cmx \
	trust_local_color_analysis.cmx author_text_analysis.cmx \
	contribution_analysis.cmx revcount_analysis.cmx \
	intertime_analysis.cmx \
	trust_origin_analysis.cmx revs_to_files_analysis.cmx \
	trust_for_online_analysis.cmx word_frequency.cmx \
	page_factory.cmx do_eval.cmx evalwiki.cmx 

# Generate_Reputation computes the reputation histories

OUR_GENERATE_REPUTATION_OBJS = timeconv.cmo textbuf.cmo text.cmo \
	read_robots.cmo \
	eval_defs.cmo rephist.cmo evaltypes.cmo wikidata.cmo \
	computestats.cmo computerep.cmo generate_reputation.cmo 

OUR_OPT_GENERATE_REPUTATION_OBJS = timeconv.cmx textbuf.cmx text.cmx \
	read_robots.cmx \
	eval_defs.cmx rephist.cmx  evaltypes.cmx wikidata.cmx \
	computestats.cmx computerep.cmx generate_reputation.cmx 

OUR_FIND_USERNAMES_OBJS = textbuf.cmo eval_defs.cmo evaltypes.cmo \
	wikidata.cmo extract_usernames.cmo


OUR_OPT_FIND_USERNAMES_OBJS = textbuf.cmx eval_defs.cmx evaltypes.cmx \
	wikidata.cmx extract_usernames.cmx

# Splitwiki splits a wiki xml file into smaller files. 
OUR_SPLITWIKI_OBJS = splitwiki.cmo
OUR_OPT_SPLITWIKI_OBJS = splitwiki.cmx

OUR_EXTRACTWIKI_OBJS = extract_wiki_subset.cmo
OUR_OPT_EXTRACTWIKI_OBJS = extract_wiki_subset.cmx

CHDIFF_OBJS = textbuf.cmo text.cmo prioq.cmo coda.cmo editlist.cmo chdiff.cmo

TEXT_OBJS = textbuf.cmo text.cmo

TIMECONV_OBJS = timeconv.cmo

FILESYSTEMSTORE_OBJS = filesystem_store.cmo

REVISION_STORE_OBJS = online_types.cmo revision_store.cmo

# Online objects.
OUR_ONLINE_OBJS = online_log.cmo online_db.cmo \
	online_revision.cmo db_page.cmo online_page.cmo event_feed.cmo  \
	online_command_line.cmo updater.cmo

OUR_OPT_ONLINE_OBJS = online_log.cmx online_db.cmx \
	online_revision.cmx db_page.cmx online_page.cmx event_feed.cmx \
	online_command_line.cmx updater.cmx

evalwiki: $(OUR_EVALWIKI_OBJS)
	$(OCAMLC) -linkpkg -o evalwiki $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_EVALWIKI_OBJS)

libs:	$(OUR_EVALWIKI_OBJS)
	$(OCAMLC) -a -o evalwiki.cma $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_EVALWIKI_OBJS)

evalwikiopt: $(OUR_OPT_EVALWIKI_OBJS)
	$(OCAMLOPT) -linkpkg -o evalwiki $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_EVALWIKI_OBJS)

libsopt: $(OUR_OPT_EVALWIKI_OBJS)
	$(OCAMLOPT) -a -o evalwiki.cmxa $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_EVALWIKI_OBJS)

generate_reputation:  $(OUR_GENERATE_REPUTATION_OBJS)
	$(OCAMLC) -linkpkg -o generate_reputation $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_GENERATE_REPUTATION_OBJS)

generate_reputationopt:  $(OUR_OPT_GENERATE_REPUTATION_OBJS)
	$(OCAMLOPT) -linkpkg -o generate_reputation $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_GENERATE_REPUTATION_OBJS)

find_usernames:  $(OUR_FIND_USERNAMES_OBJS)
	$(OCAMLC) -linkpkg -o extract_usernames $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_FIND_USERNAMES_OBJS)

find_usernamesopt:  $(OUR_OPT_FIND_USERNAMES_OBJS)
	$(OCAMLOPT) -linkpkg -o extract_usernames $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_FIND_USERNAMES_OBJS)

# splitwiki is used to split a wiki xml dump file into smaller files

splitwiki:  $(OUR_SPLITWIKI_OBJS)
	$(OCAMLC) -linkpkg -o splitwiki $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_SPLITWIKI_OBJS)

splitwikiopt:  $(OUR_OPT_SPLITWIKI_OBJS)
	$(OCAMLOPT) -linkpkg -o splitwiki $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_SPLITWIKI_OBJS)

# extract_wiki_subset is used to extract a given subset of wiki page ids into a seperate file

extract_wiki_subset:  $(OUR_EXTRACTWIKI_OBJS)
	$(OCAMLC) -linkpkg -o extract_wiki_subset $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_EXTRACTWIKI_OBJS)

extract_wiki_subsetopt:  $(OUR_OPT_EXTRACTWIKI_OBJS)
	$(OCAMLOPT) -linkpkg -o extract_wiki_subset $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_EXTRACTWIKI_OBJS)

# chdiff computes text differences, and this is used to test it. 

chdiff: $(CHDIFF_OBJS)
	$(OCAMLC) -linkpkg -o chdiff $(OCAML_CFLAGS) $(OUR_LD_ADD) $(CHDIFF_OBJS)

# text is used to split text into words, and this is used to test it. 

text:   $(TEXT_OBJS)
	$(OCAMLC) -linkpkg -o text $(OCAML_CFLAGS) $(OUR_LD_ADD) $(TEXT_OBJS)

# combinestats is used to combine several sorted statistics files into a single one

combinestats: combinestatsfiles.ml
	$(OCAMLC) -linkpkg -o combinestats $(OCAML_CFLAGS) $(OUR_LD_ADD) combinestatsfiles.ml

combinestatsopt: combinestatsfiles.ml
	$(OCAMLOPT) -linkpkg -o combinestats $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) combinestatsfiles.ml

# timeconv is used for time conversion, and this is used to test it. 

timeconv: $(TIMECONV_OBJS)
	$(OCAMLC) -linkpkg -o timeconv $(OCAML_CFLAGS) $(OUR_LD_ADD) $(TIMECONV_OBJS)

filesystem_store: $(FILESYSTEMSTORE_OBJS)
	$(OCAMLC) -linkpkg -o filesystem_store $(OCAML_CFLAGS) $(OUR_LD_ADD) $(FILESYSTEMSTORE_OBJS)

revision_store: $(REVISION_STORE_OBJS)
	$(OCAMLC) -linkpkg -o filesystem_store $(OCAML_CFLAGS) $(OUR_LD_ADD) $(REVISION_STORE_OBJS)

online_eval: $(OUR_ONLINE_OBJS) 
	$(OCAMLC) -linkpkg -o eval_online_wiki $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) eval_online_wiki.ml 

online_evalopt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o eval_online_wiki $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) eval_online_wiki.ml

vote_revision: $(OUR_ONLINE_OBJS) 
	$(OCAMLC) -linkpkg -o vote_revision $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) vote_revision.ml 

vote_revisionopt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o vote_revision $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) vote_revision.ml

all_batch: evalwiki generate_reputation splitwiki combinestats \
	libs extract_wiki_subset find_usernames
allopt_batch: evalwikiopt generate_reputationopt splitwikiopt \
	combinestatsopt libsopt extract_wiki_subsetopt find_usernamesopt

all:
	make all_batch
	make online_eval
	make vote_revision

allopt:
	make allopt_batch
	make online_evalopt
	make vote_revisionopt

universe: all allopt 

clean_batch:
	rm -f evalwiki generate_reputation chdiff text splitwiki \
	combinestats extract_usernames extract_wiki_subset

clean:
	make clean_batch
	rm -f *.o *.cmo *.cmx *.cmi .depends run_harness \
	eval_online_wiki vote_revision

# Boilerplate code for building ocaml dependencies.

.depends: *.ml
	$(OCAMLDEP) $^ > $@

-include .depends

