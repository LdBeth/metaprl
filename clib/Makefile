# Include directories
ROOT := ..
DIR := clib
CINCLUDE := -I$(OCAMLSRC)/byterun
CFLAGS := -Wall

include $(ROOT)/mk/preface

# Library files
ifdef ENSROOT
ENSFILES :=\
	intern.c\
	extern.c
endif

LCFILES :=\
	$(ENSFILES)\
	print_symbols\
	truncate\
	putenv\
	execvp\
	exit\
	debug\
	ml_debug\
	getrusage\
	register

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

#
# Patched OCaml files
#
.PHONY: patches patch

patches: $(OCAMLSRC)/byterun/intern.c $(OCAMLSRC)/byterun/extern.c
	diff -c $(OCAMLSRC)/byterun/extern.c extern.c > extern.patch
	diff -c $(OCAMLSRC)/byterun/intern.c intern.c > intern.patch

patch:
	patch -o extern.c $(OCAMLSRC)/byterun/extern.c extern.patch
	patch -o intern.c $(OCAMLSRC)/byterun/intern.c intern.patch

# Actual makefile
include ../mk/config
include Makefile.dep
