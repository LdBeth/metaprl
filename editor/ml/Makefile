#
# MetaPRL editor.
#
DIR = editor/ml
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE = -I .\
	-I $(ROOT)/theories/boot\
	-I $(ROOT)/theories/tactic\
	-I $(ROOT)/theories/ocaml\
	-I $(ROOT)/theories/base\
	-I $(ROOT)/theories/itt\
	-I $(ROOT)/ensemble\
	-I $(ROOT)/lib\
	-I $(ROOT)/library\
	-I $(ROOT)/ocaml_debug\
	-I $(CAMLP4LIB)
#	-I $(ROOT)/theories/fol\
#	-I $(ROOT)/theories/reflect_itt\
#	-I $(ROOT)/theories/tptp\

OCAMLDEPFLAGS = -prl

########################################################################
# LOCAL FILES
########################################################################

# Common files
MLZFILES =\
	proof_sig\
	package_sig\
	display_sig\
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
	shell_sig\
	shell_rewrite\
	shell_rule\
	shell_package\
	shell_root\
	shell_p4_sig\
	shell_state\
	shell_http\
	shell

MP2FILES =\
	test\
	shell_mp\
	shell_p4\
	mp\
	mp_top\
	library_eval\
	library_test

RUNFILES =\
	mp_version\
	shell_p4\
	test\
	mp\
	library_eval\
	library_test

TOPFILES =\
	mp_version\
	test\
	shell_mp\
	mp_top

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
	$(CMXADEPS_ens)\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(CMXLIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	mp.cmxa

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
	$(ROOT)/theories/boot/boot.cmxa\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	$(ROOT)/theories/itt/itt.cmxa\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pa_op.cmx\
	$(MAIN).cmxa
#	$(ROOT)/theories/fol/fol.cmxa\
#	$(ROOT)/theories/reflect_itt/reflect_itt.cmxa\
#	$(ROOT)/theories/tptp/tptp.cmxa\

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
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/boot/boot.cma\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma
#	$(ROOT)/theories/fol/fol.cma\
#	$(ROOT)/theories/reflect_itt/reflect_itt.cma\
#	$(ROOT)/theories/tptp/tptp.cma\

TCMADEPS =\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLLIB)/unix.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS_ens)\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/boot/boot.cma\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pa_op.cmo\
	$(MAIN).cma
#	$(ROOT)/theories/fol/fol.cma\
#	$(ROOT)/theories/reflect_itt/reflect_itt.cma\
#	$(ROOT)/theories/tptp/tptp.cma\

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
	$(CCLIBS_ens)

NATIVE_CCLIBS =\
	-cclib $(ROOT)/lib/clib-native$(EXT_LIB)\
	$(CCLIBS_ens)

# Name of main librariws
MAIN = meta-prl
MP = mp

all: $(MAIN).cma $(MP).top $(MP).run
opt: $(MAIN).cmxa $(MP).opt
install:: all

$(MP).run: $(CMOBJS) $(RCMOFILES) $(CMADEPS) $(THREADSLIB)
	$(RM) $@
	$(OCAMLMKTOP) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(CMOBJS) $(RCMOFILES) $(BYTE_CCLIBS) $(LIBTHREADS)

$(MP).top: $(TCMOBJS) $(TCMOFILES) $(TCMADEPS) $(THREADSLIB)
	$(RM) $@
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(TCMOBJS) $(TCMOFILES) $(BYTE_CCLIBS) $(LIBTHREADS)

$(MP).opt: $(CMXOBJS) $(TCMXFILES) $(OPTTHREADSLIB)
	$(RM) $@
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -linkall $(THREADS) $(CMXOBJS) $(TCMXFILES) $(NATIVE_CCLIBS) $(OPTTHREADS)

clean::
	$(RM) -f *.p4* *.pp* mp_version.ml

mp_version.cmo: $(CMOBJS) $(filter-out mp_version.%, $(RCMOFILES)) $(CMADEPS) $(TCMOBJS) $(filter-out mp_version.%, $(TCMOFILES)) $(TCMADEPS) mp_version.cmi Makefile
	echo -n "let version = \"MetaPRL $(MP_VERSION):\\n\\t\\tbuild [`LC_ALL=C LANG=C date`]\\n\\t\\ton `hostname`\\n\\t\\tUses $(REFINER) Refiner_$(TERMS)\"" > mp_version.ml
	$(OCAMLC) -c $(OCAMLCFLAGS) mp_version.ml

mp_version.cmx: $(CMXOBJS) $(CMXADEPS) $(TCMXADEPS) mp_version.cmi Makefile
	echo -n "let version = \"MetaPRL $(MP_VERSION):\\n\\t\\tbuild [`LC_ALL=C LANG=C date`]\\n\\t\\ton `hostname`\\n\\t\\tUses $(REFINER) Refiner_$(TERMS)\"" > mp_version.ml
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) mp_version.ml

mp_version.cmi: mp_version.mli
	$(OCAMLC) $(OCAMLCFLAGS) mp_version.mli

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep
