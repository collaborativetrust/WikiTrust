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
PACKAGES = unix,str,vec,mapmin,hashtbl_bounded,fileinfo,intvmap,extlib,mysql,netsys,netclient,camlzip,xml-light,json-wheel,sexplib.syntax 
SYNTAX = camlp4o
OUR_LIBS = evalwiki.cma online_types.cmo online_log.cmo online_db.cmo \
	online_revision.cmo db_page.cmo online_page.cmo event_feed.cmo  \
	online_command_line.cmo updater.cmo

OUR_LIBS_OPT = evalwiki.cmxa online_types.cmx online_log.cmx online_db.cmx \
	online_revision.cmx \
	db_page.cmx online_page.cmx event_feed.cmx \
	online_command_line.cmx updater.cmx

INCLUDES = -I ../../batch/analysis -I ../../analysis

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

universe: all allopt

all: downloadwp dispatcher read_rev_text fetch_all_revs_after

allopt: downloadwpopt dispatcheropt read_rev_textopt fetch_all_revs_afteropt

# Here are the objects that we want to build. 
# We need two lines of these, one for the debugging compilation,
# one for the optimizing compilation. 
OUR_ONLINE_OBJS = tmpfile.cmo wikipedia_api.cmo

OUR_OPT_ONLINE_OBJS = tmpfile.cmx wikipedia_api.cmx

downloadwp: $(OUR_ONLINE_OBJS) downloadwp.ml
	$(OCAMLC) -linkpkg -o downloadwp $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) downloadwp.ml 

downloadwpopt: $(OUR_OPT_ONLINE_OBJS) downloadwp.ml
	$(OCAMLOPT) -linkpkg -o downloadwp $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) downloadwp.ml

read_rev_text: $(OUR_ONLINE_OBJS) read_rev_text.ml
	$(OCAMLC) -linkpkg -o read_rev_text $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) read_rev_text.ml 

read_rev_textopt: $(OUR_OPT_ONLINE_OBJS) read_rev_text.ml
	$(OCAMLOPT) -linkpkg -o read_rev_text $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) read_rev_text.ml

fetch_all_revs_after: $(OUR_ONLINE_OBJS) fetch_all_revs_after.ml
	$(OCAMLC) -linkpkg -o fetch_all_revs_after $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) fetch_all_revs_after.ml 

fetch_all_revs_afteropt: $(OUR_OPT_ONLINE_OBJS) fetch_all_revs_after.ml
	$(OCAMLOPT) -linkpkg -o fetch_all_revs_after $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) fetch_all_revs_after.ml

dispatcher: $(OUR_ONLINE_OBJS) 	
	$(OCAMLC) -linkpkg -o dispatcher $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) server_coloring_dispatcher.ml 

dispatcheropt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o dispatcher $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) server_coloring_dispatcher.ml


clean:
	rm -f *.o *.cmo *.cmx *.cmi .depends dispatcher

# Boilerplate code for building ocaml dependencies.

.depends: *.ml
	$(OCAMLDEP) $^ > $@

-include .depends

