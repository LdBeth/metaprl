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
	precedence\
	opname\
	term\
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
	refine\
	dform\
	dform_print\
	theory\
	resource

# Name of library
MAIN := refiner

all: $(MAIN).cma
opt: $(MAIN).cmxa

# Actual makefile
include ../mk/config
include Makefile.dep
