# Flags
DIR := refiner/term_std
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig -I ../term_gen

# Library files
include Files
MLPFILES := $(TERM_STD_PFILES)
MLZFILES := term_std_sig

LMFILES := $(TERM_STD_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# Run Eli's preprocessor
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

clean::
	$(RM) *_simp.* *_verb.*

Makefile.dep: Files

