# Include directories
ROOT := ..
DIR := clib
CFLAGS := -Wall

include $(ROOT)/mk/preface

LCFILES :=\
	print_symbols\
	truncate\
	putenv\
	execvp\
	profile\
	exit\
	debug\
	ml_debug\
	termsize\
	locale

# Name of library
MAIN := clib

all: $(ROOT)/lib/$(MAIN)-byte$(EXT_LIB)

opt: $(ROOT)/lib/$(MAIN)-native$(EXT_LIB)

install:: $(MAIN)-byte$(EXT_LIB) $(MAIN)-native$(EXT_LIB)
	$(INSTALL) $(MAIN)-byte$(EXT_LIB) $(MPLIB)
	$(INSTALL) $(MAIN)-native$(EXT_LIB) $(MPLIB)

clean::
	$(RM) *-byte.c *-native.c

#
# Installation
#
$(ROOT)/lib/$(MAIN)-byte$(EXT_LIB): $(MAIN)-byte$(EXT_LIB)
	$(LN) $(ROOT)/$(DIR)/$(MAIN)-byte$(EXT_LIB) $@

$(ROOT)/lib/$(MAIN)-native$(EXT_LIB): $(MAIN)-native$(EXT_LIB)
	$(LN) $(ROOT)/$(DIR)/$(MAIN)-native$(EXT_LIB) $@

#
# Patched OCaml files
#
.PHONY: patches patch

ifneq ($(OCAMLSRC),undefined)
CINCLUDE := -I$(OCAMLSRC)/byterun

patches: $(OCAMLSRC)/byterun/intern.c $(OCAMLSRC)/byterun/extern.c
	diff -c $(OCAMLSRC)/byterun/extern.c extern.c > extern.patch
	diff -c $(OCAMLSRC)/byterun/intern.c intern.c > intern.patch

patch:
	patch -o extern.c $(OCAMLSRC)/byterun/extern.c extern.patch
	patch -o intern.c $(OCAMLSRC)/byterun/intern.c intern.patch
endif

# Actual makefile
include ../mk/rules
include Makefile.dep
