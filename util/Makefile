OCAMLC = ocamlc
OCAMLLEX = ocamllex

OCAMLDEP = misc.cmo ocamldep.cmo

ifndef LOCALBIN
   LOCALBIN := /usr/local/bin
endif

.PHONY: all install clean depend

all: ocamldep

install: ocamldep
	install ocamldep $(LOCALBIN)

ocamldep: $(OCAMLDEP)
	$(OCAMLC) -o $@ $(OCAMLDEP)

%.cmo: %.ml
	$(OCAMLC) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) -c $*.mli

ocamldep.ml: ocamldep.mll
	$(OCAMLLEX) ocamldep.mll

clean:
	rm -f *.cm* *~ ocamldep.ml

depend:

ocamldep.cmo: misc.cmi
