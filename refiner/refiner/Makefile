# Flags
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_ds

# Library files
LMFILES :=\
	rewrite\
	refine\
	refiner_std\
	refiner_ds\
	refiner

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
