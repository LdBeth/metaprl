# Include directories
INCLUDE :=

# Library files
LCFILES :=\
	putenv

LMFILES :=\
	punix

# Name of library
MAIN := clib

all: $(MAIN).a $(MAIN).cma
opt: $(MAIN).a $(MAIN).cmxa

install:: $(MAIN).a $(MAIN).cma
	$(INSTALL) $(MAIN).a $(MAIN).cma $(NLLIB)

# Actual makefile
include ../mk/config
include Makefile.dep
