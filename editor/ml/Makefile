#
# Nuprl-light editor.
#
DIR := editor/ml
ROOT := ../..

INCLUDE = -I .\
	-I $(ROOT)/theories/tactic\
	-I $(ROOT)/theories/ocaml\
	-I $(ROOT)/theories/base\
	-I $(ROOT)/theories/itt\
	-I $(ROOT)/theories/czf\
	-I $(ROOT)/lib\
	-I $(CAMLP4LIB)

OCAMLDEPFLAGS := -prl

# Common files
MLZFILES :=\
	io_proof_type\
	proof_type\
	package_type\
	shell_type\
	shell_p4_type

# Library files
NLFILES :=\
	io_proof_type\
	io_proof\
	proof_type\
	proof_step\
	proof\
	proof_edit\
	package_type\
	package_info\
	package_df\
	shell_type\
	shell_null\
	shell_rewrite\
	shell_rule\
	shell_p4_type\
	shell

NL2FILES :=\
	shell_p4\
	shell_nl\
	test\
	nl\
	nl_top

RUNFILES :=\
	shell_p4\
	test\
	nl

TOPFILES :=\
	shell_nl\
	nl_top

RCMOFILES := $(addsuffix .cmo, $(RUNFILES))
TCMOFILES := $(addsuffix .cmo, $(TOPFILES))
TCMXFILES := $(addsuffix .cmx, $(TOPFILES))

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
        $(CAMLLIB)/nums.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/library.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	nl.cmxa

CMXLIBS =\
	$(CMXDEPS)

CMXADEPS =\
	$(CAMLP4LIB)/odyl.cmxa\
	$(CAMLLIB)/unix.cmxa\
        $(CAMLLIB)/nums.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/library.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	$(ROOT)/theories/itt/itt.cmxa\
	$(ROOT)/theories/czf/czf.cmxa\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pa_op.cmx\
	$(MAIN).cmxa

CMXALIBS =\
	$(CMXADEPS)

CMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
        $(CAMLLIB)/nums.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(CAMLP4LIB)/camlp4_top.cma\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/prlc.cma\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/czf/czf.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma

TCMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
        $(CAMLLIB)/nums.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/prlc.cma\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/czf/czf.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma

CMALIBS =\
	$(CMADEPS)

TCMALIBS =\
	$(TCMADEPS)

CMOBJS =\
	$(CMALIBS)

TCMOBJS =\
	$(TCMALIBS)

CMXOBJS =\
	$(CMXALIBS)

CCLIBS =\
	-cclib -lunix\
        -cclib -lnums\
	-cclib $(ROOT)/lib/clib.a


# Name of main libraris
MAIN = nuprl-light
NL = nl

all: $(MAIN).cma $(NL).top
opt: $(MAIN).cmxa $(NL).opt
install:: all

$(NL).run: $(CMOBJS) $(RCMOFILES) $(CMADEPS)
	rm -f $@
	$(OCAMLMKTOP) -o $@ -custom -linkall $(CMOBJS) $(RCMOFILES) $(CCLIBS)

$(NL).top: $(TCMOBJS) $(TCMOFILES) $(TCMADEPS)
	rm -f $@
	$(OCAMLC) -o $@ -custom -linkall $(TCMOBJS) $(TCMOFILES) $(CCLIBS)

$(NL).opt: $(CMXOBJS) $(TCMXFILES)
	rm -f $@
	$(OCAMLOPT) -o $@ -linkall $(CMXOBJS) $(TCMXFILES) $(CCLIBS)

clean::
	$(RM) -f *.p4* *.pp*

# Real makefile
include $(ROOT)/mk/config
include Makefile.dep
