# Include directories
ROOT := ..
DIR := clib
INCLUDE :=

# Library files
LCFILES :=\
	truncate\
	putenv\
	execvp\
	debug\
	ml_debug

# Name of library
MAIN := clib

all: $(ROOT)/lib/$(MAIN).a

opt: $(ROOT)/lib/$(MAIN).a

install:: $(MAIN).a
	$(INSTALL) $(MAIN).a $(NLLIB)

#
# Installation
#
$(ROOT)/lib/$(MAIN).a: $(MAIN).a
	$(LN) $(ROOT)/$(DIR)/$(MAIN).a $@

# Actual makefile
include ../mk/config
include Makefile.dep
