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

PRLC :=\
	clib\
	mllib\
	refiner\
	library\
	filter

DIRS :=\
	clib\
	mllib\
	refiner\
	library\
	filter\
	theories/tactic\
	theories/ocaml\
	theories/base\
	editor/ml

.PHONY: all prlc install depend clean

all:
	@echo "Choose a target:"
	@echo "   prlc: makes the Nuprl-Light compiler"
	@echo "   install: install the compiler, and compile the theories"

prlc:
	@for i in $(PRLC); do\
		if (echo Making $$i...; cd $$i; $(MAKE) all); then true; else exit 1; fi;\
	done

install:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

clean:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

depend:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; touch Makefile.dep; $(MAKE) $@); then true; else exit 1; fi;\
	done
