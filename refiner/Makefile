# Flags
INCLUDE := -I ../mllib

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
	refine_sig\
	refiner_sig\
	ml_format_sig\
	mk_print_sig\
	resource

# Library files
LMFILES :=\
	rformat\
	opname\
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
	refiner_sig\
	term_std\
	term_op_std\
	term_addr_std\
	term_subst_std\
	term_man_std\
	term_shape_std\
	term_eval_std\
	term_meta_std\
	rewrite\
	refine\
	refiner_std\
	refiner\
	term_ds\
	term_addr_ds\
	term_op_ds\
	term_subst_ds\
	term_man_ds\
	term_shape_ds\
	refiner_ds\
	term_eval_ds\
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
