#
# Location
#
DIR = filter/base
ROOT = ../..

#
# Common Config
#
include $(ROOT)/mk/preface

#
# Name of main program
#
MAIN = filterlib

#
# Flags
#
INCLUDE =\
	-I .\
	-I $(ROOT)/lib\
	-I $(CAMLP4LIB)

OCAMLFLAGS = -pp "camlp4o$(EXE) q_MLast.cmo pa_extend.cmo"

# Type files
MLZFILES =\
	filter_type\
	filter_summary_type\
	filter_proof_type

include Files
LMFILES := $(BASE_FILES)

all: lib

#
# Included files
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Infix file is preprocessed.
#
infix.ml: infix.pre.ml
	$(RM) $@
	camlp4$(EXE) pa_o.cmo pa_op.cmo pr_o.cmo pa_extend.cmo q_MLast.cmo ./infix.pre.ml |\
		sed -e 's,"KEYWORD",keyword,' > infix.ml
	chmod 444 $@

#
# More rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

clean::
	$(RM) infix.ml
