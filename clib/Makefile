# Include directories
ROOT := ..
DIR := clib
INCLUDE :=

include $(ROOT)/mk/preface

# Library files
LCFILES :=\
	truncate\
	putenv\
	execvp\
	exit\
	debug\
	ml_debug\
	getrusage

# Name of library
MAIN := clib

all: $(ROOT)/lib/$(MAIN)$(EXT_LIB)

opt: $(ROOT)/lib/$(MAIN)$(EXT_LIB)

install:: $(MAIN)$(EXT_LIB)
	$(INSTALL) $(MAIN)$(EXT_LIB) $(MPLIB)

#
# Installation
#
$(ROOT)/lib/$(MAIN)$(EXT_LIB): $(MAIN)$(EXT_LIB)
	$(LN) $(ROOT)/$(DIR)/$(MAIN)$(EXT_LIB) $@

# Actual makefile
include ../mk/config
include Makefile.dep
