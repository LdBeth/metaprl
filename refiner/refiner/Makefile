# Flags
DIR = refiner/refiner
ROOT = ../..
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_gen -I ../term_ds -I ../rewrite

# Library files
include Files
LMFILES := $(REFINER_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# C preprocessor
OCAMLFLAGS = -pp "$(CPP) $(INCLUDE)"

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
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	rm -f refiner.ml

Makefile.dep: Files

