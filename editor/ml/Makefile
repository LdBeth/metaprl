#
# MetaPRL editor.
#
DIR = editor/ml
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE = -I .\
	-I $(ROOT)/theories/tactic\
	-I $(ROOT)/theories/ocaml\
	-I $(ROOT)/theories/base\
	-I $(ROOT)/theories/itt\
	-I $(ROOT)/theories/fol\
	-I $(ROOT)/theories/reflect_itt\
	-I $(ROOT)/theories/tptp\
	-I $(ROOT)/ensemble\
	-I $(ROOT)/lib\
	-I $(ROOT)/library\
	-I $(ROOT)/ocaml_debug\
	-I $(ENSROOT)/lib/$(HOSTTYPE)\
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
MPFILES =\
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

MP2FILES =\
	shell_p4\
	shell_mp\
	test\
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
	shell_mp\
	test\
	mp_top

RCMOFILES = $(addsuffix .cmo, $(RUNFILES))
TCMOFILES = $(addsuffix .cmo, $(TOPFILES))
TCMXFILES = $(addsuffix .cmx, $(TOPFILES))

########################################################################
# ENSEMBLE
########################################################################

ifdef ENSROOT
    CMADEPS_ens =\
        $(ENSROOT)/lib/$(HOSTTYPE)/socket.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/crypto.cma\
	$(ENSROOT)/lib/$(HOSTTYPE)/_nulldynlink.cmo\
        $(ENSROOT)/lib/$(HOSTTYPE)/libenscore.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensmin.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensrest.cma

    CMXADEPS_ens =\
        $(ENSROOT)/lib/$(HOSTTYPE)/socket.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/crypto.cmxa\
	$(ENSROOT)/lib/$(HOSTTYPE)/_nulldynlink.cmx\
        $(ENSROOT)/lib/$(HOSTTYPE)/libenscore.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensmin.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensrest.cmxa

    CMALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cma

    CMXALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cmxa

    CCLIBS_ens =\
        -ccopt -L$(ENSROOT)/lib/$(HOSTTYPE)\
        -cclib -lsock\
        -cclib -lcryptoc
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
	$(ROOT)/lib/ldsymbols.cmxa\
	$(ROOT)/lib/prlc.cmxa\
	$(CMXALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	$(ROOT)/theories/itt/itt.cmxa\
	$(ROOT)/theories/fol/fol.cmxa\
	$(ROOT)/theories/reflect_itt/reflect_itt.cmxa\
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
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/ldsymbols.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/fol/fol.cma\
	$(ROOT)/theories/reflect_itt/reflect_itt.cma\
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
	$(ROOT)/lib/library.cma\
	$(ROOT)/lib/ldsymbols.cma\
	$(ROOT)/lib/prlc.cma\
	$(CMALIBS_ens)\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/fol/fol.cma\
	$(ROOT)/theories/reflect_itt/reflect_itt.cma\
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

# Name of main librariws
MAIN = meta-prl
MP = mp

all: $(MAIN).cma $(MP).top # $(MP).run
opt: $(MAIN).cmxa $(MP).opt
install:: all

$(MP).run: $(CMOBJS) $(RCMOFILES) $(CMADEPS)
	$(RM) $@
	$(OCAMLMKTOP) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(CMOBJS) $(RCMOFILES) $(CCLIBS) $(LIBTHREADS)

$(MP).top: $(TCMOBJS) $(TCMOFILES) $(TCMADEPS)
	$(RM) $@
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -custom -linkall $(THREADS) $(TCMOBJS) $(TCMOFILES) $(CCLIBS) $(LIBTHREADS)

$(MP).opt: $(CMXOBJS) $(TCMXFILES)
	$(RM) $@
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -linkall $(THREADS) $(CMXOBJS) $(TCMXFILES) $(CCLIBS) $(OPTTHREADS)

clean::
	$(RM) -f *.p4* *.pp* mp_version.ml

mp_version.cmo: $(CMOBJS) $(filter-out mp_version.%, $(RCMOFILES)) $(CMADEPS) $(TCMOBJS) $(filter-out mp_version.%, $(TCMOFILES)) $(TCMADEPS) mp_version.cmi
	echo -n "let version = \"MetaPRL 0.5:\\n\\t\\tbuild [`date`]\\n\\t\\ton `hostname`\"" > mp_version.ml
	$(OCAMLC) -c $(OCAMLCFLAGS) mp_version.ml

mp_version.cmx: $(CMXOBJS) mp_version.cmi
	echo -n "let version = \"MetaPRL 0.5:\\n\\t\\tbuild [`date`]\\n\\t\\ton `hostname`\"" > mp_version.ml
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) mp_version.ml

mp_version.cmi: mp_version.mli
	$(OCAMLC) $(OCAMLCFLAGS) mp_version.mli

# Real makefile
include $(ROOT)/mk/config
include Makefile.dep
