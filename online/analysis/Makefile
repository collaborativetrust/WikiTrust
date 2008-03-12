## Basic Makefile to put together a testing setup for the online_db stuff
## Ian Pye

OCAMLFIND := ocamlfind
OCAMLC := ocamlc -g

OCAMLINCS := -I ../../batch/analysis 
OCAMLPACKAGES := -package dbi.mysql -package extlib


TARGETS	:= online_types-h  \
	online_types \
	online_db-h \
	online_db  \
	run_harness

all:	$(TARGETS)

online_types-h: online_types.mli
	$(OCAMLC) $<

online_types: online_types.ml
	$(OCAMLC) -c $<

online_db-h: online_db.mli
	$(OCAMLC) $(OCAMLINCS) $^ 

online_db: online_db.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLPACKAGES) $(OCAMLINCS) -c $^ 

run_harness: online_db.cmo online_db_run_harness.ml
	$(OCAMLFIND) $(OCAMLC) -o $@ $(OCAMLPACKAGES) $(OCAMLINCS) -linkpkg $^

clean:
	rm -f $(TARGETS)
	rm -f *.cmi *.cmo *~
