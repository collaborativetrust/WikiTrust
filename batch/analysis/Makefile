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
OCAML_LDA_LIBS =  
OCAML_LDA_LIBS_OPT =   
OUR_LD_ADD = str.cma unix.cma $(OCAML_LDA_LIBS)
OUR_OPTLD_ADD = str.cmxa unix.cmxa $(OCAML_LDA_LIBS_OPT)
#INCLUDES = -package xml-light -package vec -package mapmin -package hashtbl_bounded -package fileinfo -package intvmap
INCLUDES = -package xml-light -package unix -package ocamlldalibs

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLMKTOP=ocamlmktop

OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLDOC=ocamldoc
OCAMLFIND_C=ocamlfind ocamlcp
OCAMLFIND_COPT=ocamlfind ocamlopt

SUFFIXES= .ml .cmo .cmi .cmx

# OCAMLDEBUG = -g -p film 
OCAMLDEBUG = -g

# Flags for commands, depending on optimization/debugging
OCAML_CFLAGS=$(INCLUDES) $(OCAMLDEBUG) `ocamlfind query ocamlldalibs`/ocamlldalibs.cma
OCAMLOPT_FLAGS=$(INCLUDES) `ocamlfind query ocamlldalibs`/ocamlldalibs.cmxa

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

OUR_EVALWIKI_OBJS = text.cmo textbuf.cmo prioq.cmo \
	coda.cmo editlist.cmo chdiff.cmo compute_edlist.cmo timeconv.cmo \
	eval_constants.cmo rephist.cmo revision.cmo prune_analysis.cmo \
	linear_analysis.cmo circbuf_analysis.cmo reputation_analysis.cmo \
	compute_trust.cmo trust_analysis.cmo trust_local_color_analysis.cmo \
	contribution_analysis.cmo revcount_analysis.cmo intertime_analysis.cmo \
	trust_origin_analysis.cmo revs_to_files_analysis.cmo \
	page_factory.cmo do_eval.cmo evalwiki.cmo

OUR_OPT_EVALWIKI_OBJS = text.cmx textbuf.cmx prioq.cmx \
	coda.cmx editlist.cmx chdiff.cmx compute_edlist.cmx timeconv.cmx \
	eval_constants.cmx revision.cmx rephist.cmx prune_analysis.cmx \
	linear_analysis.cmx circbuf_analysis.cmx reputation_analysis.cmx \
	compute_trust.cmx trust_analysis.cmx trust_local_color_analysis.cmx \
	contribution_analysis.cmx revcount_analysis.cmx intertime_analysis.cmx \
	trust_origin_analysis.cmx revs_to_files_analysis.cmx \
	page_factory.cmx do_eval.cmx evalwiki.cmx 


# Generate_Reputation computes the reputation histories

OUR_GENERATE_REPUTATION_OBJS = timeconv.cmo text.cmo \
	eval_constants.cmo rephist.cmo evaltypes.cmo wikidata.cmo \
	computestats.cmo computerep.cmo generate_reputation.cmo 

OUR_OPT_GENERATE_REPUTATION_OBJS = timeconv.cmx text.cmx \
	eval_constants.cmx rephist.cmx  evaltypes.cmx wikidata.cmx \
	computestats.cmx computerep.cmx generate_reputation.cmx 

# Splitwiki splits a wiki xml file into smaller files. 
OUR_SPLITWIKI_OBJS = splitwiki.cmo
OUR_OPT_SPLITWIKI_OBJS = splitwiki.cmx

CHDIFF_OBJS = text.cmo prioq.cmo coda.cmo chdiff.cmo

TEXT_OBJS = text.cmo

TIMECONV_OBJS = timeconv.cmo


# Evaluates a wiki 

evalwiki: $(OUR_EVALWIKI_OBJS)
	$(OCAMLC) -o evalwiki $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_EVALWIKI_OBJS) -linkpkg

libs:	$(OUR_EVALWIKI_OBJS)
	$(OCAMLC) -a -o evalwiki.cma $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_EVALWIKI_OBJS)

evalwikiopt: $(OUR_OPT_EVALWIKI_OBJS)
	$(OCAMLOPT) -o evalwiki $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_EVALWIKI_OBJS)

libsopt: $(OUR_OPT_EVALWIKI_OBJS)
	$(OCAMLOPT) -a -o evalwiki.cmxa $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_EVALWIKI_OBJS)

generate_reputation:  $(OUR_GENERATE_REPUTATION_OBJS)
	$(OCAMLC) -o generate_reputation $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_GENERATE_REPUTATION_OBJS) -linkpkg

generate_reputationopt:  $(OUR_OPT_GENERATE_REPUTATION_OBJS)
	$(OCAMLOPT) -o generate_reputation $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_GENERATE_REPUTATION_OBJS)

# splitwiki is used to split a wiki xml dump file into smaller files

splitwiki:  $(OUR_SPLITWIKI_OBJS)
	$(OCAMLC) -o splitwiki $(OCAML_CFLAGS) $(OUR_LD_ADD) $(OUR_SPLITWIKI_OBJS)

splitwikiopt:  $(OUR_OPT_SPLITWIKI_OBJS)
	$(OCAMLOPT) -o splitwiki $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) $(OUR_OPT_SPLITWIKI_OBJS)

# chdiff computes text differences, and this is used to test it. 

chdiff: $(CHDIFF_OBJS)
	$(OCAMLC) -o chdiff $(OCAML_CFLAGS) $(OUR_LD_ADD) $(CHDIFF_OBJS)

# text is used to split text into words, and this is used to test it. 

text:   $(TEXT_OBJS)
	$(OCAMLC) -o text $(OCAML_CFLAGS) $(OUR_LD_ADD) $(TEXT_OBJS)

# combinestats is used to combine several sorted statistics files into a single one

combinestats: combinestatsfiles.ml
	$(OCAMLC) -o combinestats $(OCAML_CFLAGS) $(OUR_LD_ADD) combinestatsfiles.ml

combinestatsopt: combinestatsfiles.ml
	$(OCAMLOPT) -o combinestats $(OCAMLOPT_FLAGS) $(OUR_OPTLD_ADD) combinestatsfiles.ml

# timeconv is used for time conversion, and this is used to test it. 

timeconv: $(TIMECONV_OBJS)
	$(OCAMLC) -o timeconv $(OCAML_CFLAGS) $(OUR_LD_ADD) $(TIMECONV_OBJS)

all: evalwiki generate_reputation splitwiki combinestats libs
allopt: evalwikiopt generate_reputationopt splitwikiopt combinestatsopt
universe: all allopt

#
# Boilerplate code for building ocaml dependencies, and
# "make clean".
#

clean:
	rm -f *.o *.cmo *.cmx *.cmi .depends evalwiki generate_reputation chdiff text splitwiki combinestats

.depends: *.ml
	$(OCAMLDEP) $^ > $@

-include .depends

