# Include directories
ROOT := ..
INCLUDE :=

# Library files
LCFILES :=\
	truncate\
	putenv\
	execvp\
	debug\
	ml_debug

LMFILES :=\
	punix

# Name of library
MAIN := clib

all: $(ROOT)/lib/$(MAIN).a $(ROOT)/lib/$(MAIN).cma

opt: $(ROOT)/lib/$(MAIN).a $(ROOT)/lib/$(MAIN).cmxa

install:: $(MAIN).a $(MAIN).cma
	$(INSTALL) $(MAIN).a $(MAIN).cma $(NLLIB)

#
# Installation
#
$(ROOT)/lib/$(MAIN).a: $(MAIN).a
	$(INSTALL) $(MAIN).a $@

$(ROOT)/lib/$(MAIN).cma: $(MAIN).cma
	$(INSTALL) $(MAIN).cma $@

$(ROOT)/lib/$(MAIN).cmxa: $(MAIN).cmxa
	$(INSTALL) $(MAIN).cmxa $@

# Actual makefile
include ../mk/config
include Makefile.dep
