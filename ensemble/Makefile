# Flags
DIR := ensemble
ROOT := ..

include $(ROOT)/mk/preface

INCLUDE :=\
	-I .\
	-I $(ENSROOT)/lib\
	-I $(ROOT)/lib

#
# .mlz files
#
MLZFILES :=\
	remote_sig\
	thread_refiner_sig

# Library files
LMFILES =\
	thread_refiner_sig

ifdef ENSROOT
    LMFILES_ens =\
	$(LMFILES)\
	appl_closure\
	ensemble_queue\
	remote_sig\
	remote_null\
	remote_ensemble\
	thread_refiner_ens

    CMOFILES_ens = $(addsuffix .cmo, $(LMFILES_ens))
    CMXFILES_ens = $(addsuffix .cmx, $(LMFILES_ens))
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

all: lib

#
# Ensemble library
#
ifdef ENSROOT
ENS_LIB = $(LIB)_ens.cma
OPT_ENS_LIB = $(LIB)_ens.cmxa
RENS_LIB = $(ROOT)/lib/$(ENS_LIB)
ROPT_ENS_LIB = $(ROOT)/lib/$(OPT_ENS_LIB) $(ROOT)/lib/$(LIB)_ens$(EXT_LIB)

$(RENS_LIB): $(ENS_LIB)
	ln -sf $(ROOT)/$(DIR)/$(ENS_LIB) $@

$(ROOT)/lib/$(OPT_ENS_LIB): $(OPT_ENS_LIB)
	ln -sf $(ROOT)/$(DIR)/$(OPT_ENS_LIB) $@

$(ROOT)/lib/$(LIB)_ens$(EXT_LIB): $(LIB)_ens$(EXT_LIB)
	ln -s $(ROOT)/$(DIR)/$(LIB)_ens$(EXT_LIB) $@

$(LIB)_ens$(EXT_LIB): $(OPT_ENS_LIB)

$(ENS_LIB): $(CMOFILES_ens) thread_refiner.cmi thread_refiner_ens_mod.ml
	$(RM) thread_refiner.ml
	ln -sf thread_refiner_ens_mod.ml thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(LCMOFILES) $(CMOFILES_ens) thread_refiner.cmo

$(OPT_ENS_LIB): $(CMXFILES_ens) thread_refiner.cmi thread_refiner_ens_mod.ml
	$(RM) thread_refiner.ml
	ln -sf thread_refiner_ens_mod.ml thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(LCMXFILES) $(CMXFILES_ens) thread_refiner.cmx
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
	ln -sf $(ROOT)/$(DIR)/$(NULL_LIB) $@

$(ROPT_NULL_LIB): $(OPT_NULL_LIB)
	ln -sf $(ROOT)/$(DIR)/$(OPT_NULL_LIB) $@

$(ROPT_NULL_LIBA): $(OPT_NULL_LIBA)
	ln -sf $(ROOT)/$(DIR)/$(OPT_NULL_LIBA) $@

$(NULL_LIB): $(CMOFILES_null) thread_refiner.cmi thread_refiner_null_mod.ml
	$(RM) thread_refiner.ml
	ln -sf thread_refiner_null_mod.ml thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c thread_refiner.ml
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(LCMOFILES) $(CMOFILES_null) thread_refiner.cmo

$(OPT_NULL_LIB): $(CMXFILES_null) thread_refiner.cmi thread_refiner_null_mod.ml
	$(RM) thread_refiner.ml
	ln -sf thread_refiner_null_mod.ml thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c thread_refiner.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $@ $(LCMXFILES) $(CMXFILES_null) thread_refiner.cmx

#
# Default way to make the thread_refiner.ml
#
thread_refiner.ml: thread_refiner_null.ml
	ln -sf thread_refiner_null.ml $@

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

lib: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMAFILES) $(RNULL_LIB) $(RENS_LIB)
opt: $(RMLIFILES) $(RCMIFILES) $(RMLIFILES2) $(RCMIFILES2) $(RCMXAFILES) $(ROPT_NULL_LIB) $(ROPT_ENS_LIB)
	@$(MAKE) $(ROPT_NULL_LIBA)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MAIN).cmxa $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	$(RM) thread_refiner.ml