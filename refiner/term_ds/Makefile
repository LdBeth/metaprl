# Flags
ROOT := ../..
INCLUDE := -I $(ROOT)/mllib -I ../refbase -I ../refsig -I ../term_gen

# Library files
include Files
MLPFILES := $(TERM_DS_PFILES)
MLZFILES := term_ds_sig
LMFILES := $(TERM_DS_FILES)

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
