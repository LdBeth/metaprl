#
# Location
#
DIR = filter
ROOT = ..

#
# Common Config
#
include $(ROOT)/mk/preface

#
# Name of main program
#
MAINLIB = prlc
MAINPROG = prlc

INSTALL_LIBS = $(MAINLIB)

#
# Flags
#
INCLUDE =\
	-I $(ROOT)/lib\
	-I $(CAMLP4LIB)

OCAMLFLAGS = -pp "camlp4o$(EXE) q_MLast.cmo pa_extend.cmo"

########################################################################
# Recursive make
#########################################################################

LIBDIRS :=\
	base\
	boot\
	phobos

DIRS := $(LIBDIRS)\
	filter

SUB_CMALIBS := \
	$(addsuffix /filterlib.cma, $(LIBDIRS))\
	filter/term_grammar.cmo\

include base/Files
include boot/Files
include phobos/Files

SUB_CMXFILES :=\
	$(addprefix base/, $(addsuffix .cmx, $(BASE_FILES)))\
	$(addprefix boot/, $(addsuffix .cmx, $(BOOT_FILES)))\
	$(addprefix phobos/, $(addsuffix .cmx, $(PHOBOS_FILES)))\
	filter/term_grammar.cmx\

.PHONY: allbin optbin

all: lib
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB).cma
	@ $(MAKE) allbin

opt: optlib
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB).cmxa
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB)$(EXT_LIB)
	@ $(MAKE) optbin

profile: optlib
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB).cmxa
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB)$(EXT_LIB)

#
# Libraries
#
$(MAINLIB).cma: $(SUB_CMALIBS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAINLIB).cma $(SUB_CMALIBS)

$(MAINLIB)$(EXT_LIB): $(MAINLIB).cmxa

$(MAINLIB).cmxa: $(SUB_CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $(MAINLIB).cmxa $(SUB_CMXFILES)

#
# Standard build rules
#
lib::
	@for i in $(DIRS); do\
		if (echo Making filter/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

optlib::
	@for i in $(DIRS); do\
		if (echo Making filter/$$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

install::
	@for i in $(DIRS); do\
		if (echo Making filter/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

clean::
	@for i in $(DIRS); do\
		if (echo Cleaning filter/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

depend::
	@$(RM) Makefile.dep
	@for i in $(DIRS); do\
		if (echo Making filter/$$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done

########################################################################
# MAIN PROGRAMS
########################################################################

MFILES_CAMLP4 =\
	filter/filter_patt\
	filter/filter_prog\
	filter/filter_parse\
	filter/filter_main\

MFILES_PRLC =\
	base/filter_magic\
	filter/prlcomp\

MFILES_PRLCX =\
	filter/prlcomp\
	filter/filter_prog\
	filter/filter_bin\

MFILES_CONVERT =\
	filter/filter_convert

CMOFILES_CAMLP4 = $(addsuffix .cmo, $(MFILES_CAMLP4))
CMXFILES_CAMLP4 = $(addsuffix .cmx, $(MFILES_CAMLP4))
CMOFILES_PRLC = $(addsuffix .cmo, $(MFILES_PRLC))
CMXFILES_PRLC = $(addsuffix .cmx, $(MFILES_PRLC))
CMOFILES_PRLCX = $(addsuffix .cmo, $(MFILES_PRLCX))
CMXFILES_PRLCX = $(addsuffix .cmx, $(MFILES_PRLCX))
CMOFILES_CONVERT = $(addsuffix .cmo, $(MFILES_CONVERT))
CMXFILES_CONVERT = $(addsuffix .cmx, $(MFILES_CONVERT))

#
# Libraries for prlc
#
CMXADEPS =\
	$(ROOT)/lib/util.cmxa

CMXLIBS =\
	unix.cmxa\
	$(OPTTHREADSLIB)\
	$(CMXADEPS)

CMADEPS =\
	$(ROOT)/lib/util.cma

CMALIBS =\
	unix.cma\
	$(THREADSLIB)\
	$(CMADEPS)

BYTE_CCLIBS =\
	-cclib -lunix\
	-cclib $(ROOT)/clib/clib-byte$(EXT_LIB)\
	$(OTHER_CCLIBS)

NATIVE_CCLIBS =\
	-cclib -lunix\
	-cclib $(ROOT)/clib/clib-native$(EXT_LIB)\
	$(OTHER_CCLIBS)

#
# Libraries for camlp4n
#
CAMLP4N = camlp4n
CAMLP4O = camlp4o

STANDARD_LIBS =\
	$(CAMLLIB)/unix.cma\
	$(CAMLLIB)/str.cma\
	$(THREADSLIB)\
	$(CAMLP4LIB)/camlp4.cma\
	$(CMADEPS)\
	$(ROOT)/refiner/refiner.cma\
	$(ROOT)/library/library.cma\
	$(ROOT)/lib/trefiner_null.cma\
	$(MAINLIB).cma

STANDARDX_LIBS =\
	$(CAMLLIB)/unix.cmxa\
	$(CAMLLIB)/str.cmxa\
	$(OPTTHREADSLIB)\
	$(CAMLP4LIB)/camlp4.cmxa\
	$(CMXADEPS)\
	$(ROOT)/refiner/refiner.cmxa\
	$(ROOT)/library/library.cmxa\
	$(ROOT)/lib/trefiner_null.cmxa\
	$(MAINLIB).cmxa

CAMLP4N_CMOBJS =\
	$(CAMLP4LIB)/odyl.cma\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_dump.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(CMOFILES_CAMLP4)

CAMLP4N_CMXOBJS =\
	$(CAMLP4LIB)/odyl.cmxa\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pr_dump.cmx\
	$(CAMLP4LIB)/pa_extend.cmx\
	$(CAMLP4LIB)/q_MLast.cmx\
	$(CMXFILES_CAMLP4)

CAMLP4O_CMOBJS =\
	$(CAMLP4LIB)/odyl.cma\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pa_o.cmo\
	$(CAMLP4LIB)/pr_o.cmo\
	$(CAMLP4LIB)/pa_extend.cmo\
	$(CAMLP4LIB)/q_MLast.cmo\
	$(CMOFILES_CAMLP4)

CAMLP4O_CMXOBJS =\
	$(CAMLP4LIB)/odyl.cmxa\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pa_o.cmx\
	$(CAMLP4LIB)/pr_o.cmx\
	$(CAMLP4LIB)/pa_extend.cmx\
	$(CAMLP4LIB)/q_MLast.cmx\
	$(CMXFILES_CAMLP4)

CAMLP4X_BYTE_CCLIBS = $(BYTE_CCLIBS)
CAMLP4X_NATIVE_CCLIBS = $(NATIVE_CCLIBS)

#
# File format conversion
#
CONVERT = convert

CONVERT_CMOBJS =\
	$(STANDARD_LIBS)\
	$(CMOFILES_CONVERT)

CONVERT_CMXOBJS =\
	$(STANDARDX_LIBS)\
	$(CMXFILES_CONVERT)

CONVERT_BYTE_CCLIBS = $(BYTE_CCLIBS)
CONVERT_NATIVE_CCLIBS = $(NATIVE_CCLIBS)

#
# Files for binary compiler.
#
PRLCN = prlcn
PRLCO = prlco

PRLCN_CMOBJS =\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pr_dump.cmo\
	$(CMOFILES_PRLCX)

PRLCN_CMXOBJS =\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pr_dump.cmx\
	$(CMXFILES_PRLCX)

PRLCO_CMOBJS =\
	$(STANDARD_LIBS)\
	$(CAMLP4LIB)/pr_o.cmo\
	$(CMOFILES_PRLCX)

PRLCO_CMXOBJS =\
	$(STANDARDX_LIBS)\
	$(CAMLP4LIB)/pr_o.cmx\
	$(CMXFILES_PRLCX)

PRLCX_BYTE_CCLIBS = $(BYTE_CCLIBS)
PRLCX_NATIVE_CCLIBS = $(NATIVE_CCLIBS)

#
# Included files
#
include $(ROOT)/mk/rules

#
# Rules
#
allbin:	$(ROOT)/lib/$(CAMLP4N)$(EXE)\
	$(ROOT)/lib/$(CAMLP4O)$(EXE)\
	$(ROOT)/lib/$(PRLCN)$(EXE)\
	$(ROOT)/lib/$(PRLCO)$(EXE)\
	$(ROOT)/bin/$(MAINPROG)$(EXE)\
	$(ROOT)/bin/$(CONVERT)$(EXE)\
	$(RMLIFILES) $(RCMIFILES) $(RCMAFILES)

$(ROOT)/lib/$(CAMLP4N)$(EXE): $(CAMLP4N).run
	$(LN) ../$(DIR)/$(CAMLP4N).run $@

$(ROOT)/lib/$(CAMLP4O)$(EXE): $(CAMLP4O).run
	$(LN) ../$(DIR)/$(CAMLP4O).run $@

$(ROOT)/lib/$(PRLCN)$(EXE): $(PRLCN).run
	$(LN) ../$(DIR)/$(PRLCN).run $@

$(ROOT)/lib/$(PRLCO)$(EXE): $(PRLCO).run
	$(LN) ../$(DIR)/$(PRLCO).run $@

$(ROOT)/bin/$(MAINPROG)$(EXE): $(MAINPROG).run
	$(LN) ../$(DIR)/$(MAINPROG).run $@

$(ROOT)/bin/$(CONVERT)$(EXE): $(CONVERT).run
	$(LN) ../$(DIR)/$(CONVERT).run $@

optbin: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES) $(CAMLP4N).opt $(CAMLP4O).opt $(PRLCN).opt $(PRLCO).opt $(MAINPROG).opt $(CONVERT).opt
	$(LN) ../$(DIR)/$(CAMLP4N).opt $(ROOT)/lib/$(CAMLP4N)
	$(LN) ../$(DIR)/$(CAMLP4O).opt $(ROOT)/lib/$(CAMLP4O)
	$(LN) ../$(DIR)/$(PRLCN).opt $(ROOT)/lib/$(PRLCN)
	$(LN) ../$(DIR)/$(PRLCO).opt $(ROOT)/lib/$(PRLCO)
	$(LN) ../$(DIR)/$(MAINPROG).opt $(ROOT)/bin/$(MAINPROG)
	$(LN) ../$(DIR)/$(CONVERT).opt $(ROOT)/bin/$(CONVERT)

install:: $(MAINLIB).cma $(LCMIFILES) $(CAMLP4N).run $(CAMLP4O).run $(PRLCN).run $(PRLCO).run $(MAINPROG).run $(CONVERT).run
	$(INSTALL) $(MAINLIB).cma $(LMLIFILES) $(LCMIFILES) $(MPLIB)
	if [ $(CAMLP4N).opt -nt $(CAMLP4N).run ]; then\
		$(LN) ../$(DIR)/$(CAMLP4N).opt $(ROOT)/lib/$(CAMLP4N);\
	else\
		$(LN) ../$(DIR)/$(CAMLP4N).run $(ROOT)/lib/$(CAMLP4N);\
	fi
	if [ $(CAMLP4O).opt -nt $(CAMLP4O).run ]; then\
		$(LN) ../$(DIR)/$(CAMLP4O).opt $(ROOT)/lib/$(CAMLP4O);\
	else\
		$(LN) ../$(DIR)/$(CAMLP4O).run $(ROOT)/lib/$(CAMLP4O);\
	fi
	if [ $(PRLCN).opt -nt $(PRLCN).run ]; then\
		$(LN) ../$(DIR)/$(PRLCN).opt $(ROOT)/lib/$(PRLCN);\
	else\
		$(LN) ../$(DIR)/$(PRLCN).run $(ROOT)/lib/$(PRLCN);\
	fi
	if [ $(PRLCO).opt -nt $(PRLCO).run ]; then\
		$(LN) ../$(DIR)/$(PRLCO).opt $(ROOT)/lib/$(PRLCO);\
	else\
		$(LN) ../$(DIR)/$(PRLCO).run $(ROOT)/lib/$(PRLCO);\
	fi
	if [ $(CONVERT).opt -nt $(CONVERT).run ]; then\
		$(LN) ../$(DIR)/$(CONVERT).opt $(ROOT)/lib/$(CONVERT);\
	else\
		$(LN) ../$(DIR)/$(CONVERT).run $(ROOT)/lib/$(CONVERT);\
	fi
	$(INSTALL) $(MAINPROG).run $(MPBIN)/$(MAINPROG)

#
# Compile a special camlp4 to run Nuprl.
#
$(CAMLP4N).run: $(CAMLP4N_CMOBJS) $(THREADSLIB)
	$(OCAMLC) $(PROFILE) -o $@ -linkall -custom $(CAMLP4N_CMOBJS) $(CAMLP4X_BYTE_CCLIBS) $(LIBTHREADS)

$(CAMLP4O).run: $(CAMLP4O_CMOBJS) $(THREADSLIB)
	$(OCAMLC) $(PROFILE) -o $@ -linkall -custom $(CAMLP4O_CMOBJS) $(CAMLP4X_BYTE_CCLIBS) $(LIBTHREADS)

#
# Binary compiler
#
$(PRLCN).run: $(PRLCN_CMOBJS) $(THREADSLIB)
	$(OCAMLC) $(PROFILE) -o $@ -custom $(PRLCN_CMOBJS) $(PRLCX_BYTE_CCLIBS) $(LIBTHREADS)

$(PRLCO).run: $(PRLCO_CMOBJS) $(THREADSLIB)
	$(OCAMLC) $(PROFILE) -o $@ -custom $(PRLCO_CMOBJS) $(PRLCX_BYTE_CCLIBS) $(LIBTHREADS)

#
# Format converter
#
$(CONVERT).run: $(CONVERT_CMOBJS) $(THREADSLIB)
	$(OCAMLC) $(PROFILE) -o $@ -custom $(CONVERT_CMOBJS) $(CONVERT_BYTE_CCLIBS) $(LIBTHREADS)

#
# Compile a special camlp4 to run Nuprl.
#
$(CAMLP4N).opt: $(CAMLP4N_CMXOBJS) $(OPTTHREADSLIB)
	$(OCAMLOPT) $(PROFILE) -o $@ -linkall $(CAMLP4N_CMXOBJS) $(CAMLP4X_NATIVE_CCLIBS) $(OPTTHREADS)

$(CAMLP4O).opt: $(CAMLP4O_CMXOBJS) $(OPTTHREADSLIB)
	$(OCAMLOPT) $(PROFILE) -o $@ -linkall $(CAMLP4O_CMXOBJS) $(CAMLP4X_NATIVE_CCLIBS) $(OPTTHREADS)

#
# Binary compiler
#
$(PRLCN).opt: $(PRLCN_CMXOBJS) $(OPTTHREADSLIB)
	$(OCAMLOPT) $(PROFILE) -o $@ $(PRLCN_CMXOBJS) $(PRLCX_NATIVE_CCLIBS) $(OPTTHREADS)

$(PRLCO).opt: $(PRLCO_CMXOBJS) $(OPTTHREADSLIB)
	$(OCAMLOPT) $(PROFILE) -o $@ $(PRLCO_CMXOBJS) $(PRLCX_NATIVE_CCLIBS) $(OPTTHREADS)

#
# Format converter
#
$(CONVERT).opt: $(CONVERT_CMXOBJS) $(THREADSLIB)
	$(OCAMLOPT) $(PROFILE) -o $@ $(CONVERT_CMXOBJS) $(CONVERT_NATIVE_CCLIBS) $(OPTTHREADS)

#
# Wrapper program
#
$(MAINPROG).run: $(CMAFILES) $(CMADEPS) $(ROOT)/mk/config $(THREADSLIB) $(MAINLIB).cma $(CMOFILES_PRLC)
	$(OCAMLCP) -custom $(OCAMLCFLAGS) -o $@ $(OCAMLCPOPT) $(CMALIBS) $(CMAFILES) $(CMOFILES_PRLC) $(BYTE_CCLIBS) $(LIBTHREADS)

$(MAINPROG).opt: $(CMXAFILES) $(CMXDEPS) $(ROOT)/mk/config $(OPTTHREADSLIB) $(MAINLIB).cmxa $(CMXFILES_PRLC)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(CMXAFILES) $(CMXLIBS) $(CMXFILES_PRLC) $(PCMXFILES) $(NATIVE_CCLIBS) $(OPTTHREADS)
