# Flags
ROOT := ..

INCLUDE :=

# Types files
MLZFILES :=\
	file_base_type

# Library files
LMFILES :=\
	debug\
	env_arg\
	array_util\
	string_util\
	list_util\
	ref_util\
	file_util\
	filename_util\
	ctype\
	debug_set\
	bitset\
	dag\
	imp_dag\
	precedence\
	file_base_type\
	file_base\
	file_type_base

# Name of library
MAIN := util

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(NLLIB)

