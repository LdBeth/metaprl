#
# MetaPRL editor.
#
DIR = editor/ml
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE = -I .\
	-I $(ROOT)/lib\
	-I $(ROOT)/library\
	-I $(CAMLP4LIB)\
	-I $(ROOT)/ensemble\
	$(addprefix -I , $(MP_DIRS))\
	-I $(ROOT)/library\
	-I $(ROOT)/ocaml_debug\
	-I $(CAMLP4LIB)

OCAMLDEPFLAGS = -prl

########################################################################
# LOCAL FILES
########################################################################

# Common files
MLZFILES =\
	proof_sig\
	package_sig\
	display_sig\
	nuprl_sig\
	shell_sig\
	shell_p4_sig

# Library files
MPFILES =\
	recursive_lock\
	mux_channel\
	display_term\
	proof_edit\
	package_sig\
	package_info\
	shell_tex\
	shell_sig\
	shell_rewrite\
	shell_rule\
	shell_package\
	shell_root\
	shell_p4_sig\
	shell_http\
	shell_state\
	shell

MP2FILES =\
	shell_mp\
	shell_p4\
	nuprl_sig\
	nuprl_eval\
	nuprl_jprover\
	nuprl_run\
	mp\
	mp_top

RUNFILES =\
	mp_version\
	shell_mp\
	shell_p4\
	nuprl_sig\
	nuprl_eval\
	nuprl_jprover\
	nuprl_run\
	mp

TOPFILES =\
	mp_version\
	shell_mp\
	nuprl_sig\
	nuprl_eval\
	nuprl_jprover\
	nuprl_run\
	mp_top

ifeq ($(TESTS),YES)
TESTFILES =\
	tests/test\
	tests/prop-pigeon\

MP2FILES := $(TESTFILES) $(MP2FILES)
RUNFILES := $(TESTFILES) $(RUNFILES)
TOPFILES := $(TESTFILES) $(TOPFILES)

endif


RCMOFILES = $(addsuffix .cmo, $(RUNFILES))
TCMOFILES = $(addsuffix .cmo, $(TOPFILES))
TCMXFILES = $(addsuffix .cmx, $(TOPFILES))

########################################################################
# ENSEMBLE
########################################################################

#
# If Ensemble is configured, used the Ensemble refiner.
# Otherwise, use the null refiner.
#
ifneq ($(ENSROOT),undefined)
	CMALIBS_ens = $(ROOT)/lib/trefiner_ens.cma
	CMXALIBS_ens = $(ROOT)/lib/trefiner_ens.cmxa
else
	CMALIBS_ens = $(ROOT)/lib/trefiner_null.cma
	CMXALIBS_ens = $(ROOT)/lib/trefiner_null.cmxa
endif

########################################################################
# LIBRARIES
########################################################################

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/library.cmxa\
	$(CMXALIBS_ens)\
	$(ROOT)/lib/prlc.cmxa\
	$(addsuffix /theory.cmxa, $(MP_DIRS))\
	mp.cmxa

CMXLIBS =\
	$(CMXDEPS)

CMXADEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(OPTTHREADSLIB)\
	$(CAMLP4LIB)/odyl.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/library.cmxa\
	$(CMXALIBS_ens)\
	$(ROOT)/lib/prlc.cmxa\
	$(addsuffix /theory.cmxa, $(MP_DIRS))\
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
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(CMALIBS_ens)\
	$(ROOT)/lib/prlc.cma\
	$(addsuffix /theory.cma, $(MP_DIRS))\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma

TCMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(CMALIBS_ens)\
	$(ROOT)/lib/prlc.cma\
	$(addsuffix /theory.cma, $(MP_DIRS))\
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

BYTE_CCLIBS =\
	-cclib $(ROOT)/lib/clib-byte$(EXT_LIB)\
	$(OTHER_CCLIBS)

NATIVE_CCLIBS =\
	-cclib $(ROOT)/lib/clib-native$(EXT_LIB)\
	$(OTHER_CCLIBS)

# Name of main librariws
MAIN = meta-prl
MP = mp

all: $(MAIN).cma $(MP).top $(MP).run
opt: $(MAIN).cmxa $(MP).opt
install:: all

$(MP).run: $(CMOBJS) $(RCMOFILES) $(CMADEPS) $(THREADSLIB) $(ROOT)/lib/clib-byte$(EXT_LIB)
	$(RM) $@
	$(OCAMLMKTOP) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(CMOBJS) $(RCMOFILES) $(BYTE_CCLIBS) $(LIBTHREADS)

$(MP).top: $(TCMOBJS) $(TCMOFILES) $(TCMADEPS) $(THREADSLIB) $(ROOT)/lib/clib-byte$(EXT_LIB)
	$(RM) $@
	$(OCAMLCP) $(OCAMLCFLAGS) -o $@ -custom -linkall $(OCAMLCPOPT) $(THREADS) $(TCMOBJS) $(TCMOFILES) $(BYTE_CCLIBS) $(LIBTHREADS)

$(MP).opt: $(CMXOBJS) $(TCMXFILES) $(OPTTHREADSLIB) $(ROOT)/lib/clib-native$(EXT_LIB)
	$(RM) $@
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -linkall $(THREADS) $(CMXOBJS) $(TCMXFILES) $(NATIVE_CCLIBS) $(OPTTHREADS)

clean::
	$(RM) *.p4* *.pp* mp_version.ml
	$(RM) tests/*.p4* tests/*.pp* tests/*.cmx tests/*.cmi* tests/*.cmo* tests/*.o

mp_version.cmo: $(CMOBJS) $(filter-out mp_version.%, $(RCMOFILES)) $(CMADEPS) $(TCMOBJS) $(filter-out mp_version.%, $(TCMOFILES)) $(TCMADEPS) mp_version.cmi Makefile
	echo -n "let version = \"MetaPRL $(MP_VERSION):\\n\\t\\tbuild [`LC_ALL=C LANG=C date`]\\n\\t\\ton `hostname`\\n\\t\\tUses $(REFINER) Refiner_$(TERMS)\"" > mp_version.ml
	$(OCAMLC) -c $(OCAMLCFLAGS) mp_version.ml

mp_version.cmx: $(CMXOBJS) $(CMXADEPS) $(TCMXADEPS) mp_version.cmi Makefile
	echo -n "let version = \"MetaPRL $(MP_VERSION):\\n\\t\\tbuild [`LC_ALL=C LANG=C date`]\\n\\t\\ton `hostname`\\n\\t\\tUses $(REFINER) Refiner_$(TERMS)\"" > mp_version.ml
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) mp_version.ml

mp_version.cmi: mp_version.mli
	$(OCAMLC) $(OCAMLCFLAGS) mp_version.mli

#
# TeX
#
TEXDIR=$(ROOT)/doc/latex/theories

TEXFILES=$(addprefix $(TEXDIR)/, $(addsuffix /theory.tex, $(TEXTHEORIES)))

.PHONY: tex texbyte texopt do_tex

tex:
	+ @ if [ -f $(MP).opt ]; then $(MAKE) TEXTHEORIES="$(TEXTHEORIES)" texopt; else $(MAKE) TEXTHEORIES="$(TEXTHEORIES)" texbyte; fi

texbyte:
	+ @ $(MAKE) TEXTHEORIES="$(TEXTHEORIES)" TEXBIN=$(MP).top TEXPROG=./mptop THELIB=theory.cma do_tex

texopt:
	+ @ $(MAKE) TEXTHEORIES="$(TEXTHEORIES)" TEXBIN=$(MP).opt TEXPROG=./mpopt THELIB=theory.cmxa do_tex

do_tex: $(TEXFILES)

$(TEXDIR)/%/theory.tex: $(TEXBIN) $(TEXDIR)/%/print.ml $(ROOT)/theories/%/$(THELIB)
	$(TEXPROG) $(TEXDIR)/$*/print.ml

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep
