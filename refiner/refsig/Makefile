# Flags
ROOT := ../..
INCLUDE := -I $(ROOT)/mllib -I ../refbase

# Types files
include Files
MLZFILES := $(REFSIG_FILES)

# Library files
LMFILES := $(MLZFILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(NLLIB)

