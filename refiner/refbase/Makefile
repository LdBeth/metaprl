# Flags
ROOT := ../..
INCLUDE := -I ../../mllib

# Library files
include Files
LMFILES := $(REFBASE_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

all: lib

# Actual makefile
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(NLLIB)

