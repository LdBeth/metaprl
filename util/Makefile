ROOT = ..
DIR = util
OCAMLC = ocamlc -warn-error A
OCAMLOPT = ocamlopt -warn-error A
OCAMLLEX = ocamllex
PP=-pp "camlp4r -I $(CAMLP4LIB) -nolib" -I $(CAMLP4LIB) -linkall

include $(ROOT)/mk/preface

.PHONY: all opt install clean depend

all: $(ROOT)/lib/macropp $(ROOT)/lib/macroprint $(ROOT)/bin/ocamldep

opt: ocamldep.opt macropp.opt macroprint.opt
	ln -sf $(ROOT)/$(DIR)/ocamldep.opt $(ROOT)/bin/ocamldep
	ln -sf $(ROOT)/$(DIR)/macropp.opt $(ROOT)/lib/macropp
	ln -sf $(ROOT)/$(DIR)/macroprint.opt $(ROOT)/lib/macroprint

ocamldep.run: ocamldep.ml
	$(OCAMLC) -o $@ ocamldep.ml

ocamldep.opt: ocamldep.ml
	$(OCAMLOPT) -o $@ ocamldep.ml

macropp.run: pa_macro.ml macro_main.ml
	$(OCAMLC) $(PP) -o $@ $(CAMLP4LIB)/odyl.cma $(CAMLP4LIB)/camlp4.cma $(CAMLP4LIB)/pr_dump.cmo $(CAMLP4LIB)/pa_o.cmo pa_macro.ml macro_main.ml

macropp.opt: pa_macro.ml macro_main.ml
	$(OCAMLOPT) $(PP) -o $@ $(CAMLP4LIB)/odyl.cmxa $(CAMLP4LIB)/camlp4.cmxa $(CAMLP4LIB)/pr_dump.cmx $(CAMLP4LIB)/pa_o.cmx pa_macro.ml macro_main.ml

macroprint.run: pa_macro.ml macro_main.ml
	$(OCAMLC) $(PP) -o $@ $(CAMLP4LIB)/odyl.cma $(CAMLP4LIB)/camlp4.cma $(CAMLP4LIB)/pr_o.cmo $(CAMLP4LIB)/pa_o.cmo pa_macro.ml macro_main.ml

macroprint.opt: pa_macro.ml macro_main.ml
	$(OCAMLOPT) $(PP) -o $@ $(CAMLP4LIB)/odyl.cmxa $(CAMLP4LIB)/camlp4.cmxa $(CAMLP4LIB)/pr_o.cmx $(CAMLP4LIB)/pa_o.cmx pa_macro.ml macro_main.ml

$(ROOT)/bin/ocamldep: ocamldep.run
	ln -sf $(ROOT)/$(DIR)/$< $@

$(ROOT)/lib/macropp: macropp.run
	ln -sf $(ROOT)/$(DIR)/$< $@

$(ROOT)/lib/macroprint: macroprint.run
	ln -sf $(ROOT)/$(DIR)/$< $@

ocamldep.ml: ocamldep.mll
	$(OCAMLLEX) ocamldep.mll

clean:
	$(RM) *.cm* *~ ocamldep.ml *.o Makefile.dep *.run *.opt

depend:
