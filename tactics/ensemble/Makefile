# Flags
DIR := tactics/ensemble
ROOT := ../..

include $(ROOT)/mk/preface

#
# .mlz files
#
MLZFILES :=\
	remote_sig\

# Library files
LMFILES =\
	remote_sig\
	appl_outboard_common\
	appl_outboard_client\
	ensemble_queue
	remote_sig\
	remote_null\
	remote_ensemble\
	remote_monitor\
	thread_refiner

    CMOFILES = $(addsuffix .cmo, $(LMFILES))
    CMXFILES = $(addsuffix .cmx, $(LMFILES))

    CMALIBS =\
        $(ROOT)/lib/trefiner.cma

    CMXALIBS =\
        $(ROOT)/lib/trefiner.cmxa

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

MFILES = $(LMFILES)

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
MPSERVER = mpserver

all: lib $(MPSERVER).run

ENS_LIB = $(LIB).cma
OPT_ENS_LIB = $(LIB).cmxa
RENS_LIB = $(ROOT)/lib/$(ENS_LIB)
ROPT_ENS_LIB = $(ROOT)/lib/$(OPT_ENS_LIB) $(ROOT)/lib/$(LIB)$(EXT_LIB)

$(MPSERVER).run: $(CMIFILES_serv) $(CMOFILES_serv) $(CMADEPS_serv)
	$(OCAMLC) -custom -o $@ $(OCAMLCFLAGS) $(CMALIBS_serv) $(CMOFILES_serv) $(BYTE_CCLIBS_serv)

$(RENS_LIB): $(ENS_LIB)
	$(LN) $(ROOT)/$(DIR)/$(ENS_LIB) $@

$(ROOT)/lib/$(OPT_ENS_LIB): $(OPT_ENS_LIB)
	$(LN) $(ROOT)/$(DIR)/$(OPT_ENS_LIB) $@

$(ROOT)/lib/$(LIB)$(EXT_LIB): $(LIB)$(EXT_LIB)
	ln -s $(ROOT)/$(DIR)/$(LIB)$(EXT_LIB) $@

$(LIB)$(EXT_LIB): $(OPT_ENS_LIB)

$(ENS_LIB): $(CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(LCMOFILES) $(CMOFILES)

$(OPT_ENS_LIB): $(CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(LCMXFILES) $(CMXFILES)

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

lib: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMAFILES) $(RENS_LIB)
opt: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMXAFILES) $(ROPT_ENS_LIB)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MAIN).cmxa $(MLIFILES) $(CMIFILES) $(MPLIB)
