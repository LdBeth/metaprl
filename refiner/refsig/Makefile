# Flags
INCLUDE := -I ../../mllib -I ../refbase

# Types files
MLZFILES :=\
	term_sig\
	term_op_sig\
	term_man_sig\
	term_addr_sig\
	term_subst_sig\
	term_shape_sig\
	term_eval_sig\
	term_meta_sig\
	rewrite_sig\
	refine_sig\
	refiner_sig

# Library files
LMFILES := $(MLZFILES)

# Name of library
MAIN := lib

# Flags
CCLIBS := -cclib -lnums

all: $(MAIN).cma
opt: $(MAIN).cmxa

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(NLLIB)

# Actual makefile
include ../../mk/config
include Makefile.dep
