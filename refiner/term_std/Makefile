# Flags
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig

# Library files
LMFILES :=\
	term_std\
	term_op_std\
	term_addr_std\
	term_subst_std\
	term_man_std\
	term_shape_std\
	term_eval_std\
	term_meta_std

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
