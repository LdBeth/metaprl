# Flags
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../refiner

# Signatures
MLZFILES :=\
	ml_format_sig\
	ml_print_sig

# Library files
LMFILES :=\
	rformat\
	simple_print\
	ml_format_sig\
	ml_format\
	ml_print_sig\
	ml_print\
	ml_file\
	ml_string\
	term_template\
	term_table\
	term_stable\
	term_dtable\
	dform\
	dform_print\
	refine_exn\
	theory\
	resource

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
