# Include directories
ROOT := ..
DIR := clib
CINCLUDE := -I$(OCAMLSRC)/byterun
CFLAGS := -Wall

include $(ROOT)/mk/preface

# Library files
ifdef ENSROOT
ENSFILES :=\
	intern\
	extern
endif

LCFILES :=\
	$(ENSFILES)\
	print_symbols\
	truncate\
	putenv\
	execvp\
	profile\
	exit\
	debug\
	ml_debug\
	getrusage\
	register\
	mmap

# Name of library
MAIN := clib

all: $(ROOT)/lib/$(MAIN)-byte$(EXT_LIB)

opt: $(ROOT)/lib/$(MAIN)-native$(EXT_LIB)

install:: $(MAIN)-byte$(EXT_LIB) $(MAIN)-native$(EXT_LIB)
	$(INSTALL) $(MAIN)-byte$(EXT_LIB) $(MPLIB)
	$(INSTALL) $(MAIN)-native$(EXT_LIB) $(MPLIB)

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

patches: $(OCAMLSRC)/byterun/intern.c $(OCAMLSRC)/byterun/extern.c
	diff -c $(OCAMLSRC)/byterun/extern.c extern.c > extern.patch
	diff -c $(OCAMLSRC)/byterun/intern.c intern.c > intern.patch

patch:
	patch -o extern.c $(OCAMLSRC)/byterun/extern.c extern.patch
	patch -o intern.c $(OCAMLSRC)/byterun/intern.c intern.patch

# Actual makefile
include ../mk/rules
include Makefile.dep
