# Flags
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig

# Library files
LMFILES :=\
	term_ds\
	term_op_ds\
	term_addr_ds\
	term_subst_ds\
	term_man_ds\
	term_shape_ds\
	term_eval_ds

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
