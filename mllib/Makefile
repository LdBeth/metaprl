# Flags
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
	bitset\
	dag\
	imp_dag\
	file_base_type\
	file_base\
	file_type_base\

# Name of library
MAIN := util

all: $(MAIN).cma
opt: $(MAIN).cmxa

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(CMIFILES) $(NLLIB)

# Actual makefile
include ../mk/config
include Makefile.dep
