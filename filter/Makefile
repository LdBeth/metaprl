#
# Common config
#
include ../mk/preface

# Flags
REF_ROOT := ../
INCLUDE := -I ../clib -I ../mllib -I ../refiner -I ../library -I /usr/local/lib/camlp4
OCAMLFLAGS := -pp "camlp4o q_MLast.cmo pa_extend.cmo"

# Type files
MLZFILES :=\
	filter_type\
	filter_summary_type\
	filter_process_type\
	filter_summary_spec

# Program files
LMFILES :=\
	filter_debug\
	free_vars\
	filter_type\
	infix\
	filter_ast\
	filter_hash\
	filter_util\
	filter_ocaml\
	filter_summary\
	filter_summary_type\
	filter_summary_io\
	filter_cache\
	filter_summary_spec\
	filter_summary_util\
	term_grammar\
	filter_process_type\
	filter_parse

MFILES := filter_main

PFILES := prlcomp

# Libraries
CMXDEPS :=\
	../clib/clib.cmxa\
	../mllib/util.cmxa\
	../refiner/refiner.cmxa\
	../library/library.cmxa

CMXLIBS :=\
	unix.cmxa\
	$(CMXDEPS)

CMADEPS :=\
	../clib/clib.cma\
	../mllib/util.cma\
	../refiner/refiner.cma\
	../library/library.cma

CMALIBS :=\
	unix.cma\
	$(CMADEPS)

CCLIBS :=\
	-cclib ../clib/clib.a\
	-cclib -lunix

# Name of main program
MAIN := prlcomp

all: filter_main.cmo $(MAIN).run $(MAIN).cma
opt: filter_main.cmx $(MAIN) $(MAIN).cmxa

#
# Infix file is preprocessed.
#
infix.ml: infix.pre.ml
	rm -f $@
	camlp4 pa_o.cmo pa_op.cmo pr_o.cmo pa_extend.cmo q_MLast.cmo ./infix.pre.ml |\
		sed -e 's,"KEYWORD",keyword,' > infix.ml
	chmod 444 $@

#
# Compile a test file
#
.PHONY: test

test: test.cmiz

test.cmiz: test.mli prlcomp.run
	prlcomp.run $(INCLUDE) -c test.mli

# Included files
include ../mk/config
include Makefile.dep
