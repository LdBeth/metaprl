# Flags
DIR := refiner/reflib
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig -I ../refiner
OCAMLFLAGS := $(OCAMLFLAGS) -rectype

# Signatures
MLZFILES :=\
	simple_print_sig\
	ml_format_sig\
	ml_print_sig\
	term_compare_sig\
	ascii_io_sig\
	jlogic_sig\

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
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

