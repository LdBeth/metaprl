#
# No special includes
#
DIR := filter/phobos
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE = -I . -I $(ROOT)/lib -I $(CAMLP4LIB)

OCAMLFLAGS = -pp "camlp4o$(EXE) q_MLast.cmo pa_extend.cmo"

YACCFILES =\
	phobos_parser.mly

# Library files
include Files
LMFILES = $(PHOBOS_FILES)

# Name of main libraries
MAIN = filterlib

all: lib

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep

#
# Additional rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) Files
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) Files

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: $(YACCFILES) Files
# DO NOT DELETE
