#
# Location
#
ROOT = ..

#
# Common config
#
include $(ROOT)/mk/preface

#
# Name of main program
#
MAIN = prlc

INSTALL_LIBS = prlc

#
# Flags
#
INCLUDE =\
	-I $(ROOT)/clib\
	-I $(ROOT)/mllib\
	-I $(ROOT)/refiner/refbase\
	-I $(ROOT)/refiner/refsig\
	-I $(ROOT)/refiner/refiner\
	-I $(ROOT)/refiner/reflib\
	-I $(ROOT)/library\
	-I $(CAMLP4LIB)
OCAMLFLAGS = -pp "camlp4o$(EXE) q_MLast.cmo pa_extend.cmo"

# Type files
MLZFILES =\
	filter_type\
	filter_summary_type\
	filter_proof_type

LMFILES =\
	buffer\
	mLast_util\
	free_vars\
	filter_type\
	infix\
	filter_ast\
	filter_hash\
	filter_util\
	filter_comment\
	filter_ocaml\
	filter_exn\
	filter_summary\
	filter_summary_type\
	filter_summary_io\
	filter_summary_util\
	filter_cache_fun\
	filter_magic\
	filter_cache\
	filter_prog\
	term_grammar\
	filter_grammar

MFILES_CAMLP4 =\
	filter_parse\
	filter_main

MFILES_PRLC =\
	filter_bin

MFILES =\
	filter_parse\
	filter_main\
	filter_bin

CMOFILES_CAMLP4 = $(addsuffix .cmo, $(MFILES_CAMLP4))
CMXFILES_CAMLP4 = $(addsuffix .cmx, $(MFILES_CAMLP4))
CMOFILES_PRLC = $(addsuffix .cmo, $(MFILES_PRLC))
CMXFILES_PRLC = $(addsuffix .cmx, $(MFILES_PRLC))

PFILES =\
	filter_magic\
	prlcomp

#
# Libraries for prlc
#
CMXADEPS =\
	$(ROOT)/clib/clib.cmxa\
	$(ROOT)/mllib/util.cmxa

CMXLIBS =\
	unix.cmxa\
	$(CMXDEPS)

CMADEPS =\
	$(ROOT)/clib/clib.cma\
	$(ROOT)/mllib/util.cma

CMALIBS =\
	unix.cma\
	$(CMADEPS)

CCLIBS =\
	-cclib -lunix\
        -cclib -lnums\
	-cclib $(ROOT)/clib/clib.a

#
# Libraries for camlp4n
#
CAMLP4N = camlp4n
CAMLP4O = camlp4o
# CAMLP4LIB = /usr/local/lib/camlp4

STANDARD_LIBS =\
	$(CAMLLIB)/unix.cma\
        $(CAMLLIB)/nums.cma\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS)\
	$(ROOT)/refiner/refiner.cma\
	$(ROOT)/library/library.cma\
	$(MAIN).cma

STANDARDX_LIBS =\
	$(CAMLLIB)/unix.cmxa\
        $(CAMLLIB)/nums.cmxa\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(CMXADEPS)\
	$(ROOT)/refiner/refiner.cmxa\
	$(ROOT)/library/library.cmxa\
	$(MAIN).cmxa

CAMLP4N_CMOBJS =\
	$(CAMLP4LIB)/odyl.cma\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_dump.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(CMOFILES_CAMLP4)\
	$(CAMLP4LIB)/odyl.cmo

CAMLP4N_CMXOBJS =\
	$(CAMLP4LIB)/odyl.cmxa\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pr_dump.cmx\
	$(CAMLP4LIB)/pa_extend.cmx\
	$(CAMLP4LIB)/q_MLast.cmx\
	$(CMXFILES_CAMLP4)\
	$(CAMLP4LIB)/odyl.cmx

CAMLP4O_CMOBJS =\
	$(CAMLP4LIB)/odyl.cma\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_o.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(CMOFILES_CAMLP4)\
	$(CAMLP4LIB)/odyl.cmo

CAMLP4O_CMXOBJS =\
	$(CAMLP4LIB)/odyl.cmxa\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pr_o.cmx\
	$(CAMLP4LIB)/pa_extend.cmx\
	$(CAMLP4LIB)/q_MLast.cmx\
	$(CMXFILES_CAMLP4)\
	$(CAMLP4LIB)/odyl.cmx

CAMLP4X_CCLIBS = $(CCLIBS)

#
# Files for binary compiler.
#
PRLCN = prlcn
PRLCO = prlco

PRLCN_CMOBJS =\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pr_dump.cmo\
	$(CMOFILES_PRLC)\
	filter_bin.cmo

PRLCN_CMXOBJS =\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pr_dump.cmx\
	$(CMXFILES_PRLC)\
	filter_bin.cmx

PRLCO_CMOBJS =\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pr_o.cmo\
	$(CMOFILES_PRLC)\
	filter_bin.cmo

PRLCO_CMXOBJS =\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pr_o.cmx\
	$(CMXFILES_PRLC)\
	filter_bin.cmx

PRLCX_CCLIBS = $(CCLIBS)

all: lib
opt: all

#
# Included files
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib::	$(ROOT)/lib/$(CAMLP4N)$(EXE)\
	$(ROOT)/lib/$(CAMLP4O)$(EXE)\
	$(ROOT)/lib/$(PRLCN)$(EXE)\
	$(ROOT)/lib/$(PRLCO)$(EXE)\
	$(ROOT)/bin/$(MAIN)$(EXE)

$(ROOT)/lib/$(CAMLP4N)$(EXE): $(CAMLP4N).run
	$(INSTALL) $(CAMLP4N).run $@

$(ROOT)/lib/$(CAMLP4O)$(EXE): $(CAMLP4O).run
	$(INSTALL) $(CAMLP4O).run $@

$(ROOT)/lib/$(PRLCN)$(EXE): $(PRLCN).run
	$(INSTALL) $(PRLCN).run $@

$(ROOT)/lib/$(PRLCO)$(EXE): $(PRLCO).run
	$(INSTALL) $(PRLCO).run $@

$(ROOT)/bin/$(MAIN)$(EXE): $(MAIN).run
	$(INSTALL) $(MAIN).run $@

old_opt: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES) $(CAMLP4N).opt $(CAMLP4O).opt $(PRLCN).opt $(PRLCO).opt $(MAIN).opt
	$(INSTALL) $(CAMLP4N).opt $(ROOT)/lib/$(CAMLP4N)
	$(INSTALL) $(CAMLP4O).opt $(ROOT)/lib/$(CAMLP4O)
	$(INSTALL) $(PRLCN).opt $(ROOT)/lib/$(PRLCN)
	$(INSTALL) $(PRLCO).opt $(ROOT)/lib/$(PRLCO)
	$(INSTALL) $(MAIN).opt $(ROOT)/bin/$(MAIN)

install:: $(MAIN).cma $(LCMIFILES) $(CAMLP4N).run $(CAMLP4O).run $(PRLCN).run $(PRLCO).run $(MAIN).run
	$(INSTALL) $(MAIN).cma $(LMLIFILES) $(LCMIFILES) $(NLLIB)
	if [ $(CAMLP4N).opt -nt $(CAMLP4N).run ]; then\
		$(INSTALL) $(CAMLP4N).opt $(ROOT)/lib/$(CAMLP4N);\
	else\
		$(INSTALL) $(CAMLP4N).run $(ROOT)/lib/$(CAMLP4N);\
	fi
	if [ $(CAMLP4O).opt -nt $(CAMLP4O).run ]; then\
		$(INSTALL) $(CAMLP4O).opt $(ROOT)/lib/$(CAMLP4O);\
	else\
		$(INSTALL) $(CAMLP4O).run $(ROOT)/lib/$(CAMLP4O);\
	fi
	if [ $(PRLCN).opt -nt $(PRLCN).run ]; then\
		$(INSTALL) $(PRLCN).opt $(ROOT)/lib/$(PRLCN);\
	else\
		$(INSTALL) $(PRLCN).run $(ROOT)/lib/$(PRLCN);\
	fi
	if [ $(PRLCO).opt -nt $(PRLCO).run ]; then\
		$(INSTALL) $(PRLCO).opt $(ROOT)/lib/$(PRLCO);\
	else\
		$(INSTALL) $(PRLCO).run $(ROOT)/lib/$(PRLCO);\
	fi
	$(INSTALL) $(MAIN).run $(NLBIN)/$(MAIN)

#
# Compile a special camlp4 to run Nuprl.
#
$(CAMLP4N).run: $(CAMLP4N_CMOBJS)
	$(OCAMLC) -o $@ -linkall -custom $(CAMLP4N_CMOBJS) $(CAMLP4X_CCLIBS)

$(CAMLP4O).run: $(CAMLP4O_CMOBJS)
	$(OCAMLC) -o $@ -linkall -custom $(CAMLP4O_CMOBJS) $(CAMLP4X_CCLIBS)

#
# Binary compiler
#
$(PRLCN).run: $(PRLCN_CMOBJS)
	$(OCAMLC) -o $@ -custom $(PRLCN_CMOBJS) $(PRLCX_CCLIBS)

$(PRLCO).run: $(PRLCO_CMOBJS)
	$(OCAMLC) -o $@ -custom $(PRLCO_CMOBJS) $(PRLCX_CCLIBS)

#
# Compile a special camlp4 to run Nuprl.
#
$(CAMLP4N).opt: $(CAMLP4N_CMXOBJS)
	$(OCAMLOPT) -o $@ -linkall $(CAMLP4N_CMXOBJS) $(CAMLP4X_CCLIBS)

$(CAMLP4O).opt: $(CAMLP4O_CMXOBJS)
	$(OCAMLOPT) -o $@ -linkall -custom $(CAMLP4O_CMXOBJS) $(CAMLP4X_CCLIBS)

#
# Binary compiler
#
$(PRLCN).opt: $(PRLCN_CMXOBJS)
	$(OCAMLOPT) -o $@ -custom $(PRLCN_CMXOBJS) $(PRLCX_CCLIBS)

$(PRLCO).opt: $(PRLCO_CMXOBJS)
	$(OCAMLOPT) -o $@ -custom $(PRLCO_CMXOBJS) $(PRLCX_CCLIBS)

#
# Infix file is preprocessed.
#
infix.ml: infix.pre.ml
	rm -f $@
	camlp4$(EXE) pa_o.cmo pa_op.cmo pr_o.cmo pa_extend.cmo q_MLast.cmo ./infix.pre.ml |\
		sed -e 's,"KEYWORD",keyword,' > infix.ml
	chmod 444 $@

#
# Compile a test file
#
.PHONY: test

test: test.cmiz

test.cmiz: test.mli prlcomp.run
	prlcomp.run $(INCLUDE) -c test.mli

#
# More rules
#
lib:: $(RMLIFILES) $(RCMIFILES)	$(RCMAFILES)

clean::
	rm -f filter_type.ml filter_summary_type.ml filter_process_type.ml filter_type.mli filter_summary_type.mli filter_process_type.mli filter_summary_spec.mli filter_summary_spec.ml infix.ml
