# Flags
DIR := refiner/term_ds
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig -I ../term_gen
OCAMLFLAGS := $(OCAMLFLAGS) -rectype

# Library files
include Files
MLPFILES := $(TERM_DS_PFILES)
MLZFILES := term_ds_sig
LMFILES := $(TERM_DS_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# Use C preprocessor
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

