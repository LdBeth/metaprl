# Flags
DIR = refiner/refiner
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_gen -I ../term_ds -I ../rewrite

# Library files
include Files
LMFILES := $(REFINER_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# C preprocessor
OCAMLFLAGS = -pp "$(CAMLP4MACRO) -I../refsig"

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

refiner.ml: $(ROOT)/mk/config
	echo "module Refiner = Refiner_$(TERMS).Refiner" > refiner.ml

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	rm -f refiner.ml

Makefile.dep: Files

