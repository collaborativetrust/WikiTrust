# Copyright (c) 2007-2008 The Regents of the University of California
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
OCAML_LDA_LIBS = xml-light.cma ocamlldalibs.cma evalwiki.cma
OCAML_LDA_LIBS_OPT = xml-light.cmxa ocamlldalibs.cmxa evalwiki.cmxa
OUR_LD_ADD = str.cma unix.cma mysql.cma $(OCAML_LDA_LIBS)
OUR_OPTLD_ADD = str.cmxa unix.cmxa $(OCAML_LDA_LIBS_OPT)
OUR_PACKAGES = -package extlib -package mysql -package sexplib
INCLUDES = -I ../../batch/analysis -I ../../../OcamlLdaLibs -I ../../../OcamlLdaLibs/xml-light

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKTOP=ocamlmktop

OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLDOC=ocamldoc
OCAMLFIND_C=ocamlfind ocamlc
OCAMLFIND_COPT=ocamlfind ocamlopt

SUFFIXES= .ml .cmo .cmi .cmx

# OCAMLDEBUG = -g -p film 
OCAMLDEBUG = -g

# Flags for commands, depending on optimization/debugging
OCAML_CFLAGS=$(INCLUDES) $(OCAMLDEBUG)
OCAMLOPT_FLAGS=$(INCLUDES) 

%.cmo: %.ml
	@echo '$(OCAMLFIND_C) $(OCAML_CFLAGS) $(OUR_PACKAGES) -c $<'; \
	$(OCAMLFIND_C) $(OCAML_CFLAGS) $(OUR_PACKAGES) -c $<

%.cmi: %.mli
	@echo '$(OCAMLFIND_C) $(OUR_PACKAGES) $(OCAML_CFLAGS) -c $<'; \
	$(OCAMLFIND_C) $(OUR_PACKAGES) $(OCAML_CFLAGS) -c $<

%.cmx: %.ml
	@echo '$(OCAMLFIND_COPT) $(OCAMLOPT_FLAGS) $(OUR_PACKAGES) -c $<'; \
	$(OCAMLFIND_COPT) $(OCAMLOPT_FLAGS) $(OUR_PACKAGES) -c $<

.PHONY: all allopt universe clean

# Here are the objects that we want to build. 
# We need two lines of these, one for the debugging compilation,
# one for the optimizing compilation. 
OUR_ONLINE_OBJS = online_types.cmo online_db.cmo online_revision.cmo \
	db_page.cmo online_page.cmo online_log.cmo  \

OUR_OPT_ONLINE_OBJS = online_types.cmx online_db.cmx online_revision.cmx \
	db_page.cmx online_page.cmx online_log.cmx \

# compiles the online_db stuff 
#online_db: $(OUR_ONLINE_OBJS)
#	$(OCAMLFIND_C) -o run_harness $(OCAML_CFLAGS) $(OUR_PACKAGES) $(OUR_LD_ADD) $(OUR_ONLINE_OBJS) online_db_run_harness.ml -linkpkg

#online_dbopt: $(OUR_OPT_ONLINE_OBJS)
#	$(OCAMLFIND_COPT) -o run_harness $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_PACKAGES) $(OUR_OPT_ONLINE_OBJS) online_db_run_harness.ml -linkpkg

# Creates a way to run the online_eval process
online_eval: $(OUR_ONLINE_OBJS) 
	$(OCAMLFIND_C) -o eval_online_wiki $(OCAML_CFLAGS) $(OUR_PACKAGES) $(OUR_LD_ADD) $(OUR_ONLINE_OBJS) eval_online_wiki.ml -linkpkg

online_evalopt: $(OUR_OPT_ONLINE_OBJS)
	$(OCAMLFIND_COPT) -o eval_online_wiki $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_PACKAGES) $(OUR_OPT_ONLINE_OBJS) eval_online_wiki.ml -linkpkg


all: online_eval
allopt: online_evalopt
universe: all allopt

#
# Boilerplate code for building ocaml dependencies, and
# "make clean".
#

clean:
	rm -f *.o *.cmo *.cmx *.cmi .depends run_harness eval_online_wiki

.depends: *.ml
	$(OCAMLDEP) $^ > $@

-include .depends

