# Flags
DIR := mllib
ROOT := ..

INCLUDE := -I .

# C preprocessing
OCAMLFLAGS = -pp "$(CPP)"

# Types files
MLZFILES :=\
	mp_set\
	file_base_type\
	dag\
	array_sig\
	marshal_sig\
	remote_queue_sig\
	remote_lazy_queue_sig\
	simplehash_sig\
	hash_with_gc_sig\
	weak_memo_sig\
	register

# Library files
LMFILES :=\
	$(MLZFILES)\
	mp_debug\
	mp_pervasives\
	mmap\
	mmap_pipe\
	env_arg\
	punix\
	getrusage\
	int_util\
	array_util\
	string_util\
	list_util\
	ref_util\
	file_util\
	filename_util\
	mp_id\
	large_array\
	large_weak_array\
	marshal_buf\
	marshal_shared\
	mp_big_int\
	mp_num\
	mp_ctype\
	debug_set\
	bitset\
	imp_dag\
	precedence\
	file_base\
	file_type_base\
	splay_table\
	fun_splay_set\
	red_black_set\
	red_black_test\
	hash_set\
	small_set\
	debug_string_sets\
	string_set\
	flist\
	cycle_dag\
	thread_util\
	thread_event\
	remote_queue_null\
	remote_lazy_queue\
	memo\
	linear_set\
	splay_linear_set\
	array_linear_set\
	simplehashtbl\
	hash_with_gc\
	weak_memo

# Name of library
MAIN := util

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/rules
include Makefile.dep

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES) 
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(MPLIB)

