# Flags
DIR := refiner/refsig
ROOT := ../..

include $(ROOT)/mk/preface

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

# Use C preprocessor
OCAMLFLAGS = -pp "$(CAMLP4MACRO)"

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

