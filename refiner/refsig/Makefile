# Flags
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
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(NLLIB)

#
# Special copying for term_sig.mlz
#
SIG_EDIT :=\
	-e "s,TermSimpleSig,TermSig,"\
	-e "s,level_exp_var = level_exp_var',level_exp_var,"\
	-e "s,level_exp = level_exp',level_exp,"\
	-e "s,param = param',param,"\
	-e "s,operator = operator',operator,"

term_sig.mli: term_simple_sig.mlz
	rm -f $@
	sed $(SIG_EDIT) < term_simple_sig.mlz > $@
	chmod 444 $@

term_sig.ml: term_simple_sig.mlz
	rm -f $@
	sed $(SIG_EDIT) < term_simple_sig.mlz > $@
	chmod 444 $@
