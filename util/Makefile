OCAMLC = ocamlc
OCAMLLEX = ocamllex

OCAMLDEP = misc.cmo ocamldep.cmo

.PHONY: all install clean depend

all: ocamldep
	/bin/cp -pf ocamldep ../bin

install: ocamldep
	install ocamldep $(NLBIN)

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
misc.cmo: misc.cmi
