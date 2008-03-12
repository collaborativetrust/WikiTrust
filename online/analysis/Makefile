## Basic Makefile to put together a testing setup for the online_db stuff
## Ian Pye

OCAMLFIND := ocamlfind
OCAMLC := ocamlc -g
OCAMLOPT := ocamlopt

OCAMLINCS := -I ../../batch/analysis 
OCAMLPACKAGES := -package dbi.mysql -package extlib


TARGETS	:= online_types-h  \
	online_types \
	online_db-h \
	online_db  \
	run_harness

TARGETS-OPT := online_types-hopt  \
            online_typesopt \
	    online_db-hopt \
	    online_dbopt  \
	    run_harnessopt


all:	$(TARGETS)
allopt: $(TARGETS-OPT)

online_types-h: online_types.mli
	$(OCAMLC) $<
online_types-hopt: online_types.mli
	$(OCAMLOPT) $<

online_types: online_types.ml
	$(OCAMLC) -c $<
online_typesopt: online_types.ml
	$(OCAMLOPT) -c $<

online_db-h: online_db.mli
	$(OCAMLC) $(OCAMLINCS) $^ 
online_db-hopt: online_db.mli
	$(OCAMLOPT) $(OCAMLINCS) $^ 

online_db: online_db.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLPACKAGES) $(OCAMLINCS) -c $^ 
online_dbopt: online_db.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLPACKAGES) $(OCAMLINCS) -c $^

run_harness: online_db.cmo online_db_run_harness.ml
	$(OCAMLFIND) $(OCAMLC) -o $@ $(OCAMLPACKAGES) $(OCAMLINCS) -linkpkg $^
run_harnessopt: online_db.cmx online_db_run_harness.ml
	$(OCAMLFIND) $(OCAMLOPT) -o run_harness $(OCAMLPACKAGES) $(OCAMLINCS) -linkpkg $^


clean:
	rm -f $(TARGETS)
	rm -f *.cmi *.cmo *~
	rm -f *.cmx *.o
