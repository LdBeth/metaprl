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
	filter_summary_param

# Program files
MFILES :=\
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
	filter_summary_param\
	filter_summary_modules\
	term_grammar\
	filter_process_type\
	filter_parse\
	filter_main

PFILES := prlcomp

#
# Libraries for prlc
#
CMXDEPS :=\
	../clib/clib.cmxa\
	../mllib/util.cmxa

CMXLIBS :=\
	unix.cmxa\
	$(CMXDEPS)

CMADEPS :=\
	../clib/clib.cma\
	../mllib/util.cma

CMALIBS :=\
	unix.cma\
	$(CMADEPS)

CCLIBS :=\
	-cclib -lunix\
	-cclib ../clib/clib.a

#
# Libraries for camlp4n
#
CAMLP4N := camlp4n
CAMLP4O := camlp4o
CAMLP4LIB := /usr/local/lib/camlp4

CAMLP4N_CMOBJS =\
	$(CAMLLIB)/unix.cma\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS)\
	../refiner/refiner.cma\
	../library/library.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_dump.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(addsuffix .cmo, $(MFILES))\
	$(CAMLP4LIB)/odyl.cmo

CAMLP4O_CMOBJS =\
	$(CAMLLIB)/unix.cma\
	$(CAMLP4LIB)/odyl.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS)\
	../refiner/refiner.cma\
	../library/library.cma\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_o.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(addsuffix .cmo, $(MFILES))\
	$(CAMLP4LIB)/odyl.cmo

CAMLP4X_CCLIBS := $(CCLIBS)

#
# Name of main program
#
MAIN := prlc

.PHONY: all opt install

all: $(CAMLP4N).run $(CAMLP4O).run $(MAIN).run
opt: $(CAMLP4N).opt $(CAMLP4O).opt $(MAIN).opt

install:: $(CAMLP4N).run $(CAMLP4O).run $(MAIN).run
	$(INSTALL) $(CAMLP4N).run $(CAMLP4O).run $(NLLIB)
	$(INSTALL) $(MAIN).run $(LOCALBIN)/$(MAIN)

#
# Compile a special camlp4 to run Nuprl.
#
$(CAMLP4N).run: $(CAMLP4N_CMOBJS)
	$(OCAMLC) -o $@ -custom -linkall $(CAMLP4N_CMOBJS) $(CAMLP4X_CCLIBS)

$(CAMLP4O).run: $(CAMLP4O_CMOBJS)
	$(OCAMLC) -o $@ -custom -linkall $(CAMLP4O_CMOBJS) $(CAMLP4X_CCLIBS)

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
