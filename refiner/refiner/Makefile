# Flags
DIR = refiner/refiner
ROOT = ../..
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_gen -I ../term_ds -I ../rewrite

# Library files
include Files
MLPFILES := $(REFINER_PFILES)
LMFILES := $(REFINER_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

