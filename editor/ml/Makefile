#
# Nuprl-light editor.
#
DIR = editor/ml
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE = -I .\
	-I $(ROOT)/theories/tactic\
	-I $(ROOT)/theories/ocaml\
	-I $(ROOT)/theories/base\
	-I $(ROOT)/theories/itt\
	-I $(ROOT)/theories/tptp\
	-I $(ROOT)/ensemble\
	-I $(ROOT)/lib\
	-I $(ENSROOT)/lib\
	-I $(CAMLP4LIB)

OCAMLDEPFLAGS = -prl

########################################################################
# LOCAL FILES
########################################################################

# Common files
MLZFILES =\
	io_proof_type\
	proof_type\
	package_type\
	shell_type\
	shell_p4_type

# Library files
NLFILES =\
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

NL2FILES =\
	nl_version\
	shell_p4\
	shell_nl\
	test\
	eli\
	nl\
	nl_top

RUNFILES =\
	nl_version\
	shell_p4\
	test\
	eli\
	nl

TOPFILES =\
	nl_version\
	shell_nl\
	nl_top

RCMOFILES = $(addsuffix .cmo, $(RUNFILES))
TCMOFILES = $(addsuffix .cmo, $(TOPFILES))
TCMXFILES = $(addsuffix .cmx, $(TOPFILES))

########################################################################
# ENSEMBLE
########################################################################

ifdef ENSROOT
    ifeq ($(OSTYPE), win32)
        CMADEPS_ens =\
            $(ENSROOT)/lib/socket-$(HOSTTYPE)-$(OSTYPE).cma\
            $(ENSROOT)/lib/crypto-$(HOSTTYPE)-$(OSTYPE).cma\
            $(ENSROOT)/lib/libens-$(HOSTTYPE)-$(OSTYPE).cma

        CMXADEPS_ens =\
            $(ENSROOT)/lib/socket-$(HOSTTYPE)-$(OSTYPE).cmxa\
            $(ENSROOT)/lib/crypto-$(HOSTTYPE)-$(OSTYPE).cmxa\
            $(ENSROOT)/lib/libens-$(HOSTTYPE)-$(OSTYPE).cmxa
    else
        CMADEPS_ens =\
            $(ENSROOT)/lib/socket.cma\
            $(ENSROOT)/lib/crypto.cma\
            $(ENSROOT)/lib/libens.cma

        CMXADEPS_ens =\
            $(ENSROOT)/lib/socket-$(HOSTTYPE).cmxa\
            $(ENSROOT)/lib/crypto-$(HOSTTYPE).cmxa\
            $(ENSROOT)/lib/libens-$(HOSTTYPE).cmxa
    endif

    CMALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cma

    CMXALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cmxa

    CCLIBS_ens =\
        -ccopt -L$(ENSROOT)/lib\
        -cclib -lsock-$(HOSTTYPE)\
        -cclib -lcryptoc-$(HOSTTYPE)
else
    #
    # If Ensemble is not configured, use the null refiner.
    #
    CMALIBS_ens =\
	$(ROOT)/lib/trefiner_null.cma

    CMXALIBS_ens =\
	$(ROOT)/lib/trefiner_null.cmxa
endif

########################################################################
# LIBRARIES			
########################################################################

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(CMXADEPS_ens)\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(CMXLIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	nl.cmxa

CMXLIBS =\
	$(CMXDEPS)

CMXADEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(OPTTHREADSLIB)\
	$(CAMLP4LIB)/odyl.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(CMXADEPS_ens)\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(CMXALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	$(ROOT)/theories/itt/itt.cmxa\
	$(ROOT)/theories/tptp/tptp.cmxa\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pa_op.cmx\
	$(MAIN).cmxa

CMXALIBS =\
	$(CMXADEPS)

CMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(CAMLP4LIB)/camlp4_top.cma\
	$(CMADEPS_ens)\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/tptp/tptp.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma

TCMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS_ens)\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/tptp/tptp.cma\
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
	-cclib $(ROOT)/lib/clib$(EXT_LIB)\
	$(CCLIBS_ens)

# Name of main libraris
MAIN = nuprl-light
NL = nl

all: $(MAIN).cma $(NL).top $(NL).run
opt: $(MAIN).cmxa $(NL).opt
install:: all

$(NL).run: $(CMOBJS) $(RCMOFILES) $(CMADEPS)
	rm -f $@
	$(OCAMLMKTOP) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(CMOBJS) $(RCMOFILES) $(CCLIBS) $(LIBTHREADS)

$(NL).top: $(TCMOBJS) $(TCMOFILES) $(TCMADEPS)
	rm -f $@
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(TCMOBJS) $(TCMOFILES) $(CCLIBS) $(LIBTHREADS)

$(NL).opt: $(CMXOBJS) $(TCMXFILES)
	rm -f $@
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -linkall $(THREADS) $(CMXOBJS) $(TCMXFILES) $(CCLIBS) $(OPTTHREADS)

clean::
	$(RM) -f *.p4* *.pp*

nl_version.ml: $(CMOBJS) $(filter-out nl_version.%, $(RCMOFILES)) $(CMADEPS) $(TCMOBJS) $(filter-out nl_version.%, $(TCMOFILES)) $(TCMADEPS)
	echo -n "let version = \"Nuprl-Light 2.00.1:\\n\\t\\tbuild [`date`]\\n\\t\\ton `hostname`\"" > nl_version.ml

# Real makefile
include $(ROOT)/mk/config
include Makefile.dep
