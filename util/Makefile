ROOT = ..
DIR = util
OCAMLC = ocamlc -warn-error A
OCAMLOPT = ocamlopt
OCAMLLEX = ocamllex

OCAMLDEP = misc.cmx ocamldep.cmx

.PHONY: all opt install clean depend

all: macro.cmo $(ROOT)/bin/ocamldep
opt: all

install: ocamldep
	cp -pf ocamldep $(MPBIN)

macro.cmo: macro.ml
	ocamlc -pp "camlp4o pa_extend.cmo pa_ifdef.cmo q_MLast.cmo" -I `camlp4 -where` -c macro.ml

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
	$(RM) *.cm* *~ ocamldep.ml *.o Makefile.dep ocamldep

depend:

ocamldep.cmx: misc.cmi
misc.cmx: misc.cmi
