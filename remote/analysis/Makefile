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
PACKAGES = unix,str,vec,mapmin,hashtbl_bounded,fileinfo,xml-light,intvmap,extlib,mysql,netsys,netclient,netstring,camlzip,json-wheel,json-static,sexplib.syntax
SYNTAX = camlp4o
OUR_LIBS = wikitrust_lib.cma
OUR_LIBS_OPT = wikitrust_lib.cmxa

INCLUDES = -I ../../analysis

OCAMLC=ocamlfind ocamlc -dtypes -package $(PACKAGES) -syntax $(SYNTAX)
OCAMLOPT=ocamlfind ocamlopt -package $(PACKAGES) -syntax $(SYNTAX)

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES) -syntax $(SYNTAX)
OCAMLDOC=ocamldoc

SUFFIXES= .ml .cmo .cmi .cmx

# For profiling
# OCAMLDEBUG = -g -p film 
OCAMLDEBUG = -g

# For native code profiling
# PROFILE = -p
# Avoid profiling
PROFILE =


# Flags for commands, depending on optimization/debugging
OCAML_CFLAGS=$(INCLUDES) $(OCAMLDEBUG)
OCAMLOPT_FLAGS=$(INCLUDES) $(PROFILE)

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

all: downloadwp downloadwp2xml \
	dispatcher read_rev_text fetch_all_revs_after bot_reader \
	read_revision

allopt: downloadwpopt downloadwp2xmlopt \
	dispatcheropt read_rev_textopt fetch_all_revs_afteropt \
	bot_readeropt read_revisionopt

# Here are the objects that we want to build. 
# We need two lines of these, one for the debugging compilation,
# one for the optimizing compilation. 
OUR_ONLINE_OBJS = wikipedia_api.cmo renderer.cmo

OUR_OPT_ONLINE_OBJS = wikipedia_api.cmx renderer.cmx

downloadwp: $(OUR_ONLINE_OBJS) downloadwp.ml
	$(OCAMLC) -linkpkg -o downloadwp $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) downloadwp.ml 

downloadwpopt: $(OUR_OPT_ONLINE_OBJS) downloadwp.ml
	$(OCAMLOPT) -linkpkg -o downloadwp $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) downloadwp.ml

downloadwp2xml: $(OUR_ONLINE_OBJS) downloadwp2xml.ml
	$(OCAMLC) -linkpkg -o downloadwp2xml $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) downloadwp2xml.ml 

downloadwp2xmlopt: $(OUR_OPT_ONLINE_OBJS) downloadwp2xml.ml
	$(OCAMLOPT) -linkpkg -o downloadwp2xml $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) downloadwp2xml.ml


read_rev_text: $(OUR_ONLINE_OBJS) read_rev_text.ml
	$(OCAMLC) -linkpkg -o read_rev_text $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) read_rev_text.ml 

read_rev_textopt: $(OUR_OPT_ONLINE_OBJS) read_rev_text.ml
	$(OCAMLOPT) -linkpkg -o read_rev_text $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) read_rev_text.ml

fetch_all_revs_after: $(OUR_ONLINE_OBJS) fetch_all_revs_after.ml
	$(OCAMLC) -linkpkg -o fetch_all_revs_after $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) fetch_all_revs_after.ml 

fetch_all_revs_afteropt: $(OUR_OPT_ONLINE_OBJS) fetch_all_revs_after.ml
	$(OCAMLOPT) -linkpkg -o fetch_all_revs_after $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) fetch_all_revs_after.ml

dispatcher: $(OUR_ONLINE_OBJS) server_coloring_dispatcher.ml
	$(OCAMLC) -linkpkg -o dispatcher $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) server_coloring_dispatcher.ml 

dispatcheropt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o dispatcher $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) server_coloring_dispatcher.ml

bot_reader: $(OUR_ONLINE_OBJS) 
	$(OCAMLC) -linkpkg -o bot_reader $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) bot_reader.ml 

bot_readeropt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o bot_reader $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) bot_reader.ml

read_revision: $(OUR_ONLINE_OBJS) 
	$(OCAMLC) -linkpkg -o read_revision $(OCAML_CFLAGS) $(OUR_LIBS) $(OUR_ONLINE_OBJS) read_revision.ml 

read_revisionopt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLOPT) -linkpkg -o read_revision $(OCAMLOPT_FLAGS) $(OUR_LIBS_OPT) $(OUR_OPT_ONLINE_OBJS) read_revision.ml

clean:
	rm -f *.o *.cmo *.cmx *.cmi .depends dispatcher bot_reader read_revision

# Boilerplate code for building ocaml dependencies.

.depends: *.ml
	$(OCAMLDEP) $^ > $@

-include .depends

