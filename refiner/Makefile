# Flags
INCLUDE := -I ../mllib

# Types files
MLZFILES :=\
	ml_format_sig\
	mk_print_sig\
	refine_sig\
	resource

# Library files
LMFILES :=\
	rformat\
	opname\
	term\
   term_ds\
	term_ds_simple\
	term_util\
	simple_print\
	ml_format_sig\
	ml_format\
	ml_print_sig\
	ml_print\
	ml_file\
	ml_string\
	rewrite\
	term_template\
	term_table\
	term_stable\
	term_dtable\
	refine_sig\
	refine_util\
	refine\
	dform\
	dform_print\
	refine_exn\
	theory\
	resource

# Name of library
MAIN := refiner

# Flags
CCLIBS := -cclib -lnums

all: $(MAIN).cma
opt: $(MAIN).cmxa

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(NLLIB)

# Actual makefile
include ../mk/config
include Makefile.dep
