# Flags
DIR := refiner/reflib
ROOT := ../..
INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig -I ../refiner

# Signatures
MLZFILES :=\
	ml_format_sig\
	ml_print_sig

# Library files
include Files
LMFILES := $(REFLIB_FILES)

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
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

