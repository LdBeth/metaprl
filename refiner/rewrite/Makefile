# Flags
DIR = refiner/rewrite
ROOT = ../..
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
OCAMLFLAGS = -pp "$(CPP) -I../refsig"

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

rewrite_type_sig.mlz: rewrite_types.ml
	$(CPP) -DMLZ rewrite_types.ml >$@

clean::
	$(RM) rewrite_type_sig.mlz
