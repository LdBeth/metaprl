# Flags
INCLUDE := -I ../../mllib

# Library files
LMFILES :=\
	opname

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
