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
