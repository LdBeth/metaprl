# Flags
DIR := ensemble
ROOT := ..

include $(ROOT)/mk/preface

#
# .mlz files
#
MLZFILES :=\
	remote_sig\
	thread_refiner_sig

# Library files
LMFILES =\
	thread_refiner_sig

ifeq ($(ENSROOT),undefined)
   INCLUDE :=\
	   -I .\
	   -I $(ROOT)/lib
else
    ENSFILES =\
	appl_outboard_common\
	appl_outboard_client\
	ensemble_queue

    LMFILES_ens =\
	$(MLZFILES)\
	$(LMFILES)\
	$(ENSFILES)\
	remote_sig\
	remote_null\
	remote_ensemble\
	remote_monitor\
	thread_refiner_ens

    CMOFILES_ens = $(addsuffix .cmo, $(LMFILES_ens))
    CMXFILES_ens = $(addsuffix .cmx, $(LMFILES_ens))

    CMALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cma

    CMXALIBS_ens =\
        $(ROOT)/lib/trefiner_ens.cmxa

    #
    # Server program
    #
    MSFILES =\
	appl_outboard_common\
	appl_outboard_server

    PFILES = $(MSFILES)

    CMIFILES_serv = $(addsuffix .cmi, $(MSFILES))
    CMOFILES_serv = $(addsuffix .cmo, $(MSFILES))
    CMXFILES_serv = $(addsuffix .cmx, $(MSFILES))

    CMADEPS_serv =\
	$(CAMLLIB)/unix.cma\
	$(THREADSLIB)\
        $(ENSROOT)/lib/$(HOSTTYPE)/socket.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/crypto.cma\
	$(ENSROOT)/lib/$(HOSTTYPE)/_nulldynlink.cmo\
        $(ENSROOT)/lib/$(HOSTTYPE)/libenscore.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensmin.cma\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensrest.cma\
	$(ROOT)/lib/util.cma\
	$(MAIN).cma

    CMXADEPS_serv =\
        $(ENSROOT)/lib/$(HOSTTYPE)/socket.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/crypto.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/libenscore.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensmin.cmxa\
        $(ENSROOT)/lib/$(HOSTTYPE)/libensrest.cmxa

    CMALIBS_serv = $(CMADEPS_serv)

    CMXALIBS_serv = $(CMXADEPS_serv)

    BYTE_CCLIBS_serv =\
	-cclib $(ROOT)/lib/clib-byte.a\
	-cclib -lunix\
        -ccopt -L$(ENSROOT)/lib/$(HOSTTYPE)\
        -cclib -lsock\
        -cclib -lcryptoc

    NATIVE_CCLIBS_serv =\
	-cclib $(ROOT)/lib/clib-native.a\
	-cclib -lunix\
        -ccopt -L$(ENSROOT)/lib/$(HOSTTYPE)\
        -cclib -lsock\
        -cclib -lcryptoc
   INCLUDE :=\
	   -I .\
	   -I $(ENSROOT)/lib/$(HOSTTYPE)\
	   -I $(ROOT)/lib
endif

LMFILES_null =\
	$(LMFILES)\
	thread_refiner_null

MFILES = $(LMFILES) $(LMFILES_ens) $(LMFILES_null) thread_refiner

CMOFILES_null = $(addsuffix .cmo, $(LMFILES_null))
CMXFILES_null = $(addsuffix .cmx, $(LMFILES_null))

#
# Name of the library
#
LIB = trefiner

#
# Rules
#
.PHONY: lib

#
# Ensemble library
#
ifneq ($(ENSROOT),undefined)
MPSERVER = mpserver

all: lib $(MPSERVER).run

ENS_LIB = $(LIB)_ens.cma
OPT_ENS_LIB = $(LIB)_ens.cmxa
RENS_LIB = $(ROOT)/lib/$(ENS_LIB)
ROPT_ENS_LIB = $(ROOT)/lib/$(OPT_ENS_LIB) $(ROOT)/lib/$(LIB)_ens$(EXT_LIB)

$(MPSERVER).run: $(CMIFILES_serv) $(CMOFILES_serv) $(CMADEPS_serv)
	$(OCAMLC) -custom -o $@ $(OCAMLCFLAGS) $(CMALIBS_serv) $(CMOFILES_serv) $(BYTE_CCLIBS_serv)

$(RENS_LIB): $(ENS_LIB)
	$(LN) $(ROOT)/$(DIR)/$(ENS_LIB) $@

$(ROOT)/lib/$(OPT_ENS_LIB): $(OPT_ENS_LIB)
	$(LN) $(ROOT)/$(DIR)/$(OPT_ENS_LIB) $@

$(ROOT)/lib/$(LIB)_ens$(EXT_LIB): $(LIB)_ens$(EXT_LIB)
	ln -s $(ROOT)/$(DIR)/$(LIB)_ens$(EXT_LIB) $@

$(LIB)_ens$(EXT_LIB): $(OPT_ENS_LIB)

$(ENS_LIB): $(CMOFILES_ens) thread_refiner.cmi thread_refiner_ens_mod.ml
	$(RM) thread_refiner.ml
	$(LN) thread_refiner_ens_mod.ml thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(LCMOFILES) $(CMOFILES_ens) thread_refiner.cmo

$(OPT_ENS_LIB): $(CMXFILES_ens) thread_refiner.cmi thread_refiner_ens_mod.ml
	$(RM) thread_refiner.ml
	$(LN) thread_refiner_ens_mod.ml thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(LCMXFILES) $(CMXFILES_ens) thread_refiner.cmx
else

#
# No Ensemble
#
all: lib

endif

#
# Null library
#
NULL_LIB = $(LIB)_null.cma
OPT_NULL_LIB = $(LIB)_null.cmxa
OPT_NULL_LIBA = $(LIB)_null$(EXT_LIB)
RNULL_LIB = $(ROOT)/lib/$(NULL_LIB)
ROPT_NULL_LIB = $(ROOT)/lib/$(OPT_NULL_LIB)
ROPT_NULL_LIBA = $(ROOT)/lib/$(OPT_NULL_LIBA)

$(RNULL_LIB): $(NULL_LIB)
	$(LN) $(ROOT)/$(DIR)/$(NULL_LIB) $@

$(ROPT_NULL_LIB): $(OPT_NULL_LIB)
	$(LN) $(ROOT)/$(DIR)/$(OPT_NULL_LIB) $@

$(ROPT_NULL_LIBA): $(OPT_NULL_LIBA)
	$(LN) $(ROOT)/$(DIR)/$(OPT_NULL_LIBA) $@

$(NULL_LIB): $(CMOFILES_null) thread_refiner.cmi thread_refiner_null_mod.ml
	$(RM) thread_refiner.ml
	$(LN) thread_refiner_null_mod.ml thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(LCMOFILES) $(CMOFILES_null) thread_refiner.cmo

$(OPT_NULL_LIB): $(CMXFILES_null) thread_refiner.cmi thread_refiner_null_mod.ml
	$(RM) thread_refiner.ml
	$(LN) thread_refiner_null_mod.ml thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(LCMXFILES) $(CMXFILES_null) thread_refiner.cmx

#
# Default way to make the thread_refiner.ml
#
thread_refiner.ml: thread_refiner_null.ml
	$(LN) thread_refiner_null.ml $@

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

lib: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMAFILES) $(RNULL_LIB) $(RENS_LIB)
opt: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMXAFILES) $(ROPT_NULL_LIB) $(ROPT_ENS_LIB)
	@$(MAKE) $(ROPT_NULL_LIBA)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MAIN).cmxa $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	$(RM) thread_refiner.ml
