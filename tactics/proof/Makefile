#
# No special includes
#
DIR := tactics/proof
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE = -I . -I $(CAMLP4LIB) \
	-I $(ROOT)/refiner/refsig  \
	-I $(ROOT)/refiner/refiner \
	-I $(ROOT)/$(ENSEMBLE_DIR) \

OCAMLFLAGS = -pp "camlp4o$(EXE) q_MLast.cmo"

# Type files
MLZFILES :=\
	tactic_boot_sig

# Library files
include Files
LMFILES = $(PROOF_FILES)

# Name of main libraries
MAIN = prooflib
INSTALL_LIBS = $(MAIN)

all: lib

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep

#
# Additional rules
#
lib: $(MAIN).cma $(RMLIFLES) $(RCMIFILES) $(RCMAFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES)
	@+$(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

Makefile.dep: Files

