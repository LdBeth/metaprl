#L
# MetaPRL editor.
#
DIR = editor/ml
ROOT = ../..
THEORYNAME = support
THEORYDESCR = MetaPRL Internal \"Helper\" Modules

include $(ROOT)/mk/preface

INCLUDE = -I .\
	-I $(ROOT)/lib\
	-I $(ROOT)/library\
	-I $(CAMLP4LIB)\
	-I $(ROOT)/$(ENSEMBLE_DIR)\
	$(SUPPORT_INCL)\
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
	display_sig\
	nuprl_sig\

# Library files
MPFILES =

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

ifeq ($(TESTS_ENABLED),YES)
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
# LIBRARIES
########################################################################

BASE_LIBS =\
	$(ROOT)/lib/lm\
	$(ROOT)/lib/util\
	$(ROOT)/lib/refiner\
	$(ROOT)/lib/library\
	$(ROOT)/lib/trefiner\
	$(ROOT)/lib/prooflib\

BASE_CMAS  = $(addsuffix .cma , $(BASE_LIBS))
BASE_CMXAS = $(addsuffix .cmxa, $(BASE_LIBS))

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(CAMLLIB)/str.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(BASE_CMXAS)\
	$(ROOT)/lib/prlc.cmxa\
	$(addsuffix /theory.cmxa, $(MP_DIRS))\
	mp.cmxa

CMXLIBS =\
	$(CMXDEPS)

CMXADEPS =\
	$(CAMLLIB)/unix.cmxa\
	$(CAMLLIB)/str.cmxa\
	$(OPTTHREADSLIB)\
	$(CAMLP4LIB)/odyl.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(BASE_CMXAS)\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pa_op.cmx\
	$(ROOT)/lib/prlc.cmxa\
	$(addsuffix /theory.cmxa, $(MP_DIRS))\

CMXALIBS =\
	$(CMXADEPS)

CMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(CAMLLIB)/str.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(CAMLP4LIB)/camlp4_top.cma\
	$(BASE_CMAS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(ROOT)/lib/prlc.cma\
	$(addsuffix /theory.cma, $(MP_DIRS))\

TCMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(CAMLLIB)/str.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(BASE_CMAS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(ROOT)/lib/prlc.cma\
	$(addsuffix /theory.cma, $(MP_DIRS))\

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

BYTE_CCLIBS := $(BYTE_CCLIBS)\
	-cclib $(ROOT)/lib/clib-byte$(EXT_LIB)\
	-cclib $(ROOT)/lib/liblmcutil-byte$(EXT_LIB)\
	$(OTHER_CCLIBS)

NATIVE_CCLIBS := $(NATIVE_CCLIBS)\
	-cclib $(ROOT)/lib/clib-native$(EXT_LIB)\
	-cclib $(ROOT)/lib/liblmcutil-native$(EXT_LIB)\
	$(OTHER_CCLIBS)

# Name of main librariws
MP = mp

all: $(MP).top $(MP).run
opt: $(MP).opt
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
	$(RM) *.p4* *.pp* mp_version.ml make_mp_version
	$(RM) tests/*.p4* tests/*.pp* tests/*.cmx tests/*.cmi* tests/*.cmo* tests/*.o

make_mp_version: make_mp_version.ml
	$(OCAMLC) -o $@ unix.cma $<

mp_version.cmo: $(CMOBJS) $(filter-out mp_version.%, $(RCMOFILES)) $(CMADEPS) $(TCMOBJS) $(filter-out mp_version.%, $(TCMOFILES)) $(TCMADEPS) mp_version.cmi Makefile $(ROOT)/mk/defaults make_mp_version
	./make_mp_version -version $(MP_VERSION) -refiner $(REFINER) -terms $(TERMS) > mp_version.ml
	$(OCAMLC) -c $(OCAMLCFLAGS) mp_version.ml

mp_version.cmx: $(CMXOBJS) $(CMXADEPS) $(TCMXADEPS) mp_version.cmi Makefile $(ROOT)/mk/defaults make_mp_version
	./make_mp_version -version $(MP_VERSION) -refiner $(REFINER) -terms $(TERMS) > mp_version.ml
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
	$(TEXPROG) -cli $(TEXDIR)/$*/print.ml

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep
