# Flags
DIR := mllib
ROOT := ..

INCLUDE := -I .

# Types files
MLZFILES :=\
	mp_set\
	file_base_type\
	dag\
	remote_queue_sig\
	remote_lazy_queue_sig\
	simplehash_sig

# Library files
LMFILES := $(MLZFILES)\
	mp_debug\
	mp_pervasives\
	env_arg\
	punix\
	getrusage\
	array_util\
	string_util\
	list_util\
	ref_util\
	file_util\
	filename_util\
	mp_big_int\
	mp_num\
	ctype\
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
	memo\
	thread_util\
	thread_event\
	remote_queue_null\
	remote_lazy_queue\
	bi_memo\
	simplehashtbl

# Name of library
MAIN := util

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES) 
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(MPLIB)

