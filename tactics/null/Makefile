# Flags
DIR := tactics/null
ROOT := ../..

include $(ROOT)/mk/preface

LMFILES =\
	thread_refiner\

INCLUDE :=\
	-I .\
	-I $(ROOT)/lib

#
# Name of the library
#
MAIN = trefiner
INSTALL_LIBS = $(MAIN)

#
# Rules
#
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
	@+$(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MAIN).cmxa $(MLIFILES) $(CMIFILES) $(MPLIB)
