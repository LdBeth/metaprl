# Include directories
ROOT := ..
DIR := clib
INCLUDE :=

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

all: $(ROOT)/lib/$(MAIN).a pthread

opt: $(ROOT)/lib/$(MAIN).a pthread

pthread:
	if [ -f libpthread-$(HOSTTYPE).lib ]; then\
		ln -sf $(ROOT)/$(DIR)/libpthread-$(HOSTTYPE).lib $(ROOT)/lib/libpthread.a;\
	fi

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
