DIR:=
ROOT:=.

#
# Various variables
#
include mk/preface

#
# Build all the parts of Nuprl-Light:
#    refiner: logic engine
#    filter: front end to the compiler
#    theories/tactic: tactic library
#    theories/base: root theory
#    theories/itt: Nuprl type theory
#    theories/lf: Edinburgh logical framework
#    theories/czf: Aczel's contructive set theory
#    editor/ml: interactive proof editor
#
REFINER_DIRS =\
	util\
	clib\
	mllib\
	refiner\
	library\
	debug\
	ensemble

MP_DIRS = $(addprefix theories/, $(THEORIES))

DIRS = $(REFINER_DIRS) filter $(MP_DIRS) editor/ml

.PHONY: all opt
.PHONY: profile_all profile_clean profile_byte filter profile profile_opt profile_mem profile_mem_opt
.PHONY: install depend clean check_config
.PHONY: documentation docs doc

all: check_config
	+@for i in $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

opt: check_config
	+@for i in $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

profile_clean:
	+@for i in $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i clean); then true; else exit 1; fi;\
	done

profile_all: check_config
	+@for i in $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; OCAMLCP=ocamlcp OCAMLCPOPT="-p a" $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

filter: check_config
	+@for i in $(REFINER_DIRS) filter; do\
		if (echo Making $$i...; $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

filter_opt: check_config
	+@for i in $(REFINER_DIRS) filter; do\
		if (echo Making $$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

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

profile_opt: check_config
	+@for i in $(REFINER_DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done
	+@if (echo Making filter...; $(MAKE) -C filter PROFILE=-p INLINE=0 profile); then true; else exit 1; fi
	+@for i in $(MP_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done

profile_opt_mem: check_config
	+@for i in $(REFINER_DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE="-p -compact" INLINE=0 opt); then true; else exit 1; fi;\
	done
	+@if (echo Making filter...; $(MAKE) -C filter PROFILE="-p -compact" INLINE=0 profile); then true; else exit 1; fi
	+@for i in $(MP_DIRS) editor/ml; do\
		if (echo Making $$i...; $(MAKE) -C $$i PROFILE="-p -compact" INLINE=0 opt); then true; else exit 1; fi;\
	done

install: check_config
	+@for i in $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

clean:
	+@for i in lib bin doc $(DIRS); do\
		if (echo Cleaning $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

depend: check_config
	+@$(MAKE) -C util
	+@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done
	+@$(MAKE) -C refiner depend
	+@$(MAKE) -C filter depend

mk/config: mk/make_config.sh
	@echo Making mk/config...
	@ROOT="$(ROOT)" TERMS="$(TERMS)" REFINER="$(REFINER)" MAKE_OPTS="$(MAKE_OPTS)" SEQ_SET="$(SEQ_SET)" CCC="$(CCC)" ENSROOT="$(ENSROOT)" OCAMLSRC="$(OCAMLSRC)" THEORIES="$(THEORIES)" TESTS="$(TESTS)" READLINE="$(READLINE)" mk/make_config.sh

mk/config.local:
	@touch mk/config.local

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
