DIR:=
ROOT:=.

#
# Various variables
#
include mk/preface

#
# If LIBMOJAVE is undefined, then use libmojave
#
ifeq ($(LIBMOJAVE),undefined)
   LIBMOJAVE := libmojave
endif

#
# Build all the parts of Nuprl-Light:
#    refiner: logic engine
#    filter: front end to the compiler
#    support: support library
#    theories/base: root theory
#    theories/itt: Nuprl type theory
#    theories/lf: Edinburgh logical framework
#    theories/czf: Aczel's contructive set theory
#    editor/ml: interactive proof editor
#
REFINER_DIRS :=\
	$(LIBMOJAVE)\
	clib\
	mllib\
	refiner\
	library\
	debug\
	ensemble

DEP_DIRS :=\
	$(LIBMOJAVE)\
	refiner\
	filter\

DIRS = $(REFINER_DIRS)\
	filter\
	$(MP_DIRS)\
	editor/ml

.PHONY: all opt
.PHONY: profile_all profile_clean profile_byte filter profile profile_opt profile_mem profile_mem_opt
.PHONY: install depend clean check_config
.PHONY: documentation docs doc latex theories.pdf all-theories.pdf ocaml-book

all: check_config
	+@for i in util $(DIRS); do if [ -f $$i/Makefile.prl ]; then\
		if (echo Making $$i...; $(MAKE) -C $$i -f Makefile.prl $@); then true; else exit 1; fi;\
	else\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	fi; done

opt: check_config check_versions_opt
	+@for i in util $(DIRS); do if [ -f $$i/Makefile.prl ]; then\
		if (echo Making $$i...; $(MAKE) -C $$i -f Makefile.prl $@); then true; else exit 1; fi;\
	else\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	fi; done

profile_clean:
	+@for i in util $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i clean); then true; else exit 1; fi;\
	done

profile_all: check_config
	+@for i in util $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; OCAMLCP=ocamlcp OCAMLCPOPT="-p a" $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

filter: check_config
	+@for i in util $(REFINER_DIRS) filter; do\
		if (echo Making $$i...; $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

filter_opt: check_config check_versions_opt
	+@for i in util $(REFINER_DIRS) filter; do\
		if (echo Making $$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

debug: check_config
	+@$(MAKE) OCAMLC="$(OCAMLC) -g" OCAMLMKTOP="$(OCAMLMKTOP) -g"

profile_byte: check_config
	+@$(MAKE) profile_clean
	+@$(MAKE) all
	+@$(MAKE) profile_clean
	+@$(MAKE) profile_all

profile: check_config
	+@$(MAKE) filter
	+@$(MAKE) profile_opt

profile_mem: check_config
	+@$(MAKE) filter
	+@$(MAKE) profile_opt_mem

profile_opt: check_config check_versions_opt
	+@for i in util $(REFINER_DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done
	+@if (echo Making filter...; $(MAKE) -C filter PROFILE=-p INLINE=0 profile); then true; else exit 1; fi
	+@for i in $(MP_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done

profile_opt_mem: check_config
	+@for i in util $(REFINER_DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE="-p -compact" INLINE=0 opt); then true; else exit 1; fi;\
	done
	+@if (echo Making filter...; $(MAKE) -C filter PROFILE="-p -compact" INLINE=0 profile); then true; else exit 1; fi
	+@for i in $(MP_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE="-p -compact" INLINE=0 opt); then true; else exit 1; fi;\
	done

install: check_config
	+@for i in util $(DIRS); do if [ -f $$i/Makefile.prl ]; then\
		if (echo Making $$i...; $(MAKE) -C $$i -f Makefile.prl $@); then true; else exit 1; fi;\
	else\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	fi; done

clean:
	+@for i in lib bin doc util $(DIRS); do if [ -d $$i ]; then\
		if [ -f $$i/Makefile.prl ]; then\
			if (echo Cleaning $$i...; $(MAKE) -C $$i -f Makefile.prl $@); then true; else exit 1; fi;\
		else\
			if (echo Cleaning $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
		fi\
	fi; done

depend: check_config
	+@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done
	+@for i in $(DEP_DIRS); do if [ -f $$i/Makefile.prl ]; then\
		if (echo Making $$i...; $(MAKE) -C $$i -f Makefile.prl $@); then true; else exit 1; fi;\
	else\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	fi; done

mk/config: mk/make_config.sh
	@echo Making mk/config...
	@ROOT="$(ROOT)" TERMS="$(TERMS)" REFINER="$(REFINER)" MAKE_OPTS="$(MAKE_OPTS)" SEQ_SET="$(SEQ_SET)" CCC="$(CCC)" ENSROOT="$(ENSROOT)" OCAMLSRC="$(OCAMLSRC)" THEORIES="$(THEORIES)" TESTS="$(TESTS)" READLINE="$(READLINE)" NCURSES="$(NCURSES)" LIBMOJAVE="$(LIBMOJAVE)" SLOPPY_DEPENDENCIES="$(SLOPPY_DEPENDENCIES)" mk/make_config.sh

mk/config.local:
	@touch mk/config.local

check_versions:: mk/config mk/config.local
	@if [ ! -r $(CAMLLIB)/parsetree.cmi ]; then \
		echo '!!! The file $(CAMLLIB)/parsetree.cmi does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLLIB variable'; echo '!!!';\
		exit 1;\
	fi
	@if [ ! -r $(CAMLP4LIB)/camlp4.cma ]; then \
		echo '!!! The file $(CAMLP4LIB)/camlp4.cma does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLP4LIB variable'; echo '!!!';\
		exit 1;\
	fi

check_versions_opt:: mk/config mk/config.local
	@if [ ! -r $(CAMLP4LIB)/camlp4.cmxa ]; then \
		echo '!!! The file $(CAMLP4LIB)/camlp4.cmxa does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLP4LIB variable'; echo '!!!';\
		exit 1;\
	fi

check_config::check_versions mk/config mk/config.local
	@if [ $(TERMS) != ds -a $(TERMS) != std ]; then\
		echo "ERROR: Invalid TERMS variable, edit mk/config file before running make"; \
		exit 1; \
	fi
	@if [ $(REFINER) != SIMPLE -a $(REFINER) != VERBOSE ]; then\
		echo "ERROR: Invalid REFINER variable, edit mk/config file before running make"; \
		exit 1; \
	fi
	@if [ $(SEQ_SET) != Array -a $(SEQ_SET) != Splay ]; then\
		echo "ERROR: Invalid SEQ_SET variable, edit mk/config file before running make"; \
		exit 1; \
	fi

complete:
	@cvs update && $(MAKE) filter filter_opt && $(MAKE) all opt

realclean::
	@MAKE="$(MAKE)" MAKEFLAGS="$(MAKEFLAGS)" mk/cvs_realclean.sh

docs::
	+@$(MAKE) -C doc all

doc: docs
documentation: docs

latex ocaml-book::
	+@$(MAKE) -C doc $@

theories.pdf: latex
all-theories.pdf: latex
