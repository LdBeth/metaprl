# Flags
DIR := mllib
ROOT := ..

include $(ROOT)/mk/preface

INCLUDE := -I .

# C preprocessing
OCAMLFLAGS = -pp "$(CAMLP4MACRO)"

# Types files
include Files

MLZFILES := $(MLLIB_MLZFILES)
LMFILES := $(MLLIB_FILES)

# Name of library
MAIN := util

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES)
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	$(RM) comment_parse.ml
