# Flags
DIR = refiner/rewrite
ROOT = ../..

include $(ROOT)/mk/preface

INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_gen -I ../term_ds

# Library files
include Files
MLPFILES := $(REWRITE_PFILES)
LMFILES := $(REWRITE_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# C preprocessor
OCAMLFLAGS = -pp "$(CAMLP4MACRO) -I../refsig"

all: lib

rewrite_types.mli: rewrite_types.ml
	$(OCAMLCP) $(OCAMLCFLAGS) -c -i $< > $@.tmp
	mv -f $@.tmp $@

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
	$(RM) rewrite_types.mli

Makefile.dep: Files
