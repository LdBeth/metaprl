#
# General options
#
MAKE = make -j3

#
# Environment for simp and verb variations
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
	debug

MP_DIRS =\
	filter\
	ensemble\
	theories/tactic\
	theories/ocaml\
	theories/base\
	theories/itt\
	theories/tptp\
	theories/reflect_itt\
	theories/fol

DIRS = $(REFINER_DIRS) $(MP_DIRS) editor/ml

.PHONY: all opt simp verb opt_simp opt_verb
.PHONY: profile_all profile_clean profile_byte profile profile_opt
.PHONY: install depend clean

all: verb
opt: opt_verb

simp:
	@for i in $(DIRS); do\
		if (echo Making $$i...; $(SIMP_ENV) $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

verb:
	@for i in $(DIRS); do\
		if (echo Making $$i...; $(VERB_ENV) $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

opt_simp:
	@for i in $(DIRS); do\
		if (echo Making $$i...; $(SIMP_ENV) $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

opt_verb:
	@for i in $(DIRS); do\
		if (echo Making $$i...; $(VERB_ENV) $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

profile_clean: 
	@for i in $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; $(SIMP_ENV) $(MAKE) -C $$i clean); then true; else exit 1; fi;\
	done

profile_all: 
	@for i in $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; OCAMLCP=ocamlcp OCAMLCPOPT="-p a" $(SIMP_ENV) $(MAKE) -C $$i all); then true; else exit 1; fi;\
	done

profile_byte:
	@$(MAKE) profile_clean
	@$(MAKE) all
	@$(MAKE) profile_clean
	@$(MAKE) profile_all

profile: 
	@$(MAKE) filter
	@$(MAKE) profile_opt

profile_opt:
	@for i in $(REFINER_DIRS); do\
		if (echo Making $$i...; $(SIMP_ENV) $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done
	@if (echo Making filter...; $(MAKE) -C filter PROFILE=-p INLINE=0 profile); then true; else exit 1; fi
	@for i in $(MP_DIRS) editor/ml; do\
		if (echo Making $$i...; $(SIMP_ENV) $(MAKE) -C $$i PROFILE=-p INLINE=0 opt); then true; else exit 1; fi;\
	done

install:
	@for i in $(DIRS); do\
		if (echo Making $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

clean:
	@for i in lib bin $(DIRS); do\
		if (echo Cleaning $$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

depend:
	@$(MAKE) -C util
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i && touch Makefile.dep && $(MAKE) $@); then true; else exit 1; fi;\
	done
