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
.PHONY: pthread

MAIN := clib

all: $(ROOT)/lib/$(MAIN)$(EXT_LIB) pthread

opt: $(ROOT)/lib/$(MAIN)$(EXT_LIB) pthread

pthread:
	if [ -f libpthread-$(HOSTTYPE).lib ]; then\
		ln -sf $(ROOT)/$(DIR)/libpthread-$(HOSTTYPE).lib $(ROOT)/lib/libpthread.a;\
	fi

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
