# Flags
DIR := refiner/refbase
ROOT := ../..
INCLUDE := -I ../../mllib

# Library files
include Files
LMFILES := $(REFBASE_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

all: lib

# Actual makefile
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
