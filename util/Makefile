ROOT = ..
DIR = util
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex

OCAMLDEP = misc.cmx ocamldep.cmx

.PHONY: all opt install clean depend

all: $(ROOT)/bin/ocamldep
opt: all

install: ocamldep
	cp -pf ocamldep $(MPBIN)

ocamldep: $(OCAMLDEP)
	$(OCAMLOPT) -o $@ $(OCAMLDEP)

$(ROOT)/bin/ocamldep: ocamldep
	ln -sf $(ROOT)/$(DIR)/ocamldep $@

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) -c $*.mli

ocamldep.ml: ocamldep.mll
	$(OCAMLLEX) ocamldep.mll

clean:
	rm -f *.cm* *~ ocamldep.ml *.o Makefile.dep

depend:

ocamldep.cmx: misc.cmi
misc.cmx: misc.cmi
