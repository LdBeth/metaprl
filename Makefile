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

REFINER_DIRS :=\
	util\
	clib\
	mllib\
	refiner\
	library

NL_DIRS :=\
	filter\
	theories/tactic\
	theories/ocaml\
	theories/base\
	theories/itt\
	theories/czf\

DIRS := $(REFINER_DIRS) $(NL_DIRS) editor/ml

.PHONY: all install depend clean profile

all:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

profile: 
	$(MAKE) clean
	$(MAKE) all
	@for i in $(REFINER_DIRS) editor/ml; do\
		if (echo Making $$i...; cd $$i; make clean; OCAMLCP=ocamlcp OCAMLMKTOP="ocamlcp -linkall toplevellib.cma" $(MAKE) all); then true; else exit 1; fi;\
	done

install:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

clean:
	@for i in $(DIRS) lib bin; do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

depend:
	(cd util; make)
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; touch Makefile.dep; $(MAKE) $@); then true; else exit 1; fi;\
	done
