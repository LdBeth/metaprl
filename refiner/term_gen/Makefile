# Flags
DIR := refiner/term_gen
ROOT := ../..
INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig

# Library files
include Files
MLPFILES := $(TERM_GEN_PFILES)
LMFILES := $(TERM_GEN_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# C preprocessing
OCAMLFLAGS = -pp "$(CAMLP4MACRO) -I../refsig"

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

