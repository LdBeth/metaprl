DIR:=
ROOT:=.

#
# Various variables
#
include mk/preface

#
# Build all the parts of MetaPRL:
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
	libmojave\
	clib\
	mllib\
	refiner\
	library\
	debug\
	$(ENSEMBLE_DIR)\
	tactics/proof\

DEP_DIRS :=\
	libmojave\
	refiner\
	filter\

DIRS = $(REFINER_DIRS)\
	filter\
	$(MP_DIRS)\
	editor/ml

.PHONY: all opt
.PHONY: profile_all profile_clean profile_byte filter profile profile_opt profile_mem profile_mem_opt
.PHONY: install depend clean check_config check_omake
.PHONY: documentation docs doc latex theories.pdf all-theories.pdf ocaml-book

all: check_config
	+@for i in util $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

opt: check_config check_versions_opt
	+@for i in util $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

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
	+@for i in util $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

clean:
	+@for i in lib bin doc util $(DIRS); do if [ -d $$i ]; then\
		if (echo Cleaning $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	fi; done

depend: check_config
	+@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done
	+@for i in $(DEP_DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

mk/config: mk/make_config.sh
	@echo Making mk/config...
	@ROOT="$(ROOT)" TERMS="$(TERMS)" REFINER="$(REFINER)" MAKE_OPTS="$(MAKE_OPTS)" SEQ_SET="$(SEQ_SET)" CCC="$(CCC)" ENSROOT="$(ENSROOT)" OCAMLSRC="$(OCAMLSRC)" THEORIES="$(THEORIES)" TESTS_ENABLED="$(TESTS_ENABLED)" READLINE_ENABLED="$(READLINE_ENABLED)" NCURSES_ENABLED="$(NCURSES_ENABLED)" SSL_ENABLED="$(SSL_ENABLED)" THREADS_ENABLED="$(THREADS_ENABLED)" SLOPPY_DEPENDENCIES="$(SLOPPY_DEPENDENCIES)" NATIVE_ENABLED="$(NATIVE_ENABLED)" BYTE_ENABLED="$(BYTE_ENABLED)" NATIVE_PROFILING_ENABLED="$(NATIVE_PROFILING_ENABLED)" mk/make_config.sh

mk/config.local:
	@cp -f mk/config.local.empty mk/config.local

check_omake::
	@echo '!!!'
	@if [ -e .omakedb ]; then \
		echo '!!! This directory was previously built using omake.';\
		echo '!!! Mixing omake builds with make ones is probably not a good idea.';\
		echo '!!!';\
		echo '!!! You have several choices:';\
        echo '!!!  - (recommended) continue using omake';\
        echo '!!!  - run "make realclean", remove the .omakedb file,';\
        echo '!!!    and switch to using make';\
        echo '!!!  - (not recommended) remove the .omakedb file, run "make depend"';\
        echo '!!!    and switch to using make';\
		echo '!!!';\
		exit 1;\
	else\
		echo '!!! Warning: using make for building MetaPRL is not recommended';\
		echo '!!!          Please consider using omake instead.';\
		echo '!!!';\
		sleep 1;\
	fi

check_versions:: mk/config mk/config.local check_omake
	@if [ ! -r $(CAMLLIB)/parsetree.cmi ]; then \
		echo '!!! The file $(CAMLLIB)/parsetree.cmi does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLLIB variable'; echo '!!!';\
		exit 1;\
	fi
	@if [ ! -r $(CAMLP4LIB)/camlp4.cma ] || [ ! -r $(CAMLP4LIB)/pa_op.cmo ]; then \
		echo '!!! One of the files';\
		echo '!!!    $(CAMLP4LIB)/camlp4.cma';\
		echo '!!!    $(CAMLP4LIB)/pa_op.cmo';\
		echo '!!! does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLP4LIB variable'; echo '!!!';\
		exit 1;\
	fi

check_versions_opt:: mk/config mk/config.local check_omake
	@if [ ! -r $(CAMLP4LIB)/camlp4.cmxa ] || [ ! -r $(CAMLP4LIB)/pa_op.cmx ]; then \
		echo '!!! One of the files';\
		echo '!!!    $(CAMLP4LIB)/camlp4.cmxa';\
		echo '!!!    $(CAMLP4LIB)/pa_op.cmx';\
		echo '!!! does not exist (or is not readable)!'; echo '!!!';\
		echo '!!! Please consult doc/htmlman/mp-install.html (http://metaprl.org/install.html)';\
		echo '!!! for instructions on compiling OCaml and setting the CAMLP4LIB variable'; echo '!!!';\
		exit 1;\
	fi

check_config::check_omake check_versions mk/config mk/config.local
	@if [ $(TERMS) != ds -a $(TERMS) != std ]; then\
		echo "ERROR: the TERMS variable is currenly set to an invalid value, please fix it in mk/config file"; \
		exit 1; \
	fi
	@if [ $(REFINER) != SIMPLE -a $(REFINER) != VERBOSE ]; then\
		echo "ERROR: the REFINER variable is currenly set to an invalid value, please fix it in mk/config file"; \
		exit 1; \
	fi
	@if [ $(SEQ_SET) != Lm_array -a $(SEQ_SET) != Lm_splay ]; then\
		echo "ERROR: the SEQ_SET variable is currenly set to an invalid value, please fix it in mk/config file"; \
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
