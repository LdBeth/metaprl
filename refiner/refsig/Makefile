# Flags
DIR := refiner/refsig
ROOT := ../..
INCLUDE := -I $(ROOT)/mllib -I ../refbase

# Types files
include Files
MLZFILES := $(REFSIG_FILES)

# Library files
LMFILES := $(MLZFILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

#
# Special copying for term_sig.mlz
#
SIG_EDIT :=\
	-e "s,TermSimpleSig,TermSig,"\
	-e "s,type level_exp_var =.*,type level_exp_var,"\
	-e "s,and level_exp =.*,and level_exp,"\
	-e "s,and param =.*,and param,"\
	-e "s,and operator =.*,and operator,"

term_sig.mli: term_simple_sig.mlz
	$(RM) $@
	sed $(SIG_EDIT) < term_simple_sig.mlz > $@
	chmod 444 $@

term_sig.ml: term_simple_sig.mlz
	$(RM) $@
	sed $(SIG_EDIT) < term_simple_sig.mlz > $@
	chmod 444 $@
