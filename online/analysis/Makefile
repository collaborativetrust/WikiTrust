# $Id: Makefile,v 1.1 2004/08/05 13:40:59 rwmj Exp $

OCAMLFIND := ocamlfind
OCAMLC := ocamlc

OCAMLINCS := -I ../../batch/analysis
OCAMLPACKAGES := -package dbi.mysql -package json-wheel


TARGETS	:= online_types-h  \
	online_types \
	online_db-h \
	online_db 

all:	$(TARGETS)

online_types-h: online_types.mli
	$(OCAMLC) $<

online_types: online_types.ml
	$(OCAMLC) -c $<

online_db-h: online_db.mli
	$(OCAMLC) $(OCAMLINCS) $^ 

online_db: online_db.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLPACKAGES) $(OCAMLINCS) -c $^ 
	#$(OCAMLFIND) $(OCAMLC) -o $@ $(OCAMLPACKAGES) -linkpkg $@.cmo

#debug: examplesLib.cmo debug.ml
#	$(OCAMLC) $(OCAMLINCS) $(OCAMLLIBS) $^ -o $@

clean:
	rm -f $(TARGETS)
	rm -f *.cmi *.cmo *~
