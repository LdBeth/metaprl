#
# Nuprl-light editor.
#
INCLUDE := -I . -I ../../theories/tactic -I ../../theories/ocaml -I ../../theories/base -I $(NLLIB) -I /usr/local/lib/camlp4
OCAMLDEPFLAGS := -prl

# Common files
MLZFILES :=\
	package_type\
	shell_type

# Library files
NLFILES :=\
	proof_step\
	proof\
	proof_edit\
	package_type\
	package_info\
	package_df\
	shell_type\
	shell_null\
	shell_rewrite\
	shell_p4\
	shell

MFILES :=

PFILES :=\
	nl

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
        $(CAMLLIB)/nums.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(NLLIB)/clib.cmxa\
	$(NLLIB)/util.cmxa\
	$(NLLIB)/refiner.cmxa\
	$(NLLIB)/library.cmxa\
	$(NLLIB)/refiner.cmxa\
	$(NLLIB)/prlc.cmxa\
	../../theories/tactic/tactics.cmxa\
	../../theories/ocaml/ocaml_df.cmxa\
	../../theories/base/base.cmxa\
	nl.cmxa

CMXLIBS =\
	$(CMXDEPS)

CMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
        $(CAMLLIB)/nums.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(CAMLP4LIB)/camlp4_top.cma\
	$(NLLIB)/clib.cma\
	$(NLLIB)/util.cma\
	$(NLLIB)/refiner.cma\
	$(NLLIB)/library.cma\
	$(NLLIB)/refiner.cma\
	$(NLLIB)/prlc.cma\
	../../theories/tactic/tactics.cma\
	../../theories/ocaml/ocaml_df.cma\
	../../theories/base/base.cma\
	$(MAIN).cma

CMALIBS =\
	$(CMADEPS)

CMOBJS =\
	$(CMALIBS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	nl.cmo

CCLIBS =\
	-cclib -lunix\
        -cclib -lnums\
	-cclib $(NLLIB)/clib.a


# Name of main libraris
MAIN = nuprl-light
NL = nl

all: $(MAIN).cma $(NL).run
opt: $(MAIN).cmxa $(NL)

$(NL).run: $(CMOBJS)
	$(OCAMLMKTOP) -g -o $@ -custom -linkall $(CMOBJS) $(CCLIBS)

clean::
	$(RM) -f *.p4* *.pp*

# Real makefile
include ../../mk/config
include Makefile.dep
