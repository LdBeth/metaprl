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
# More rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files
