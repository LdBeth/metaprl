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

DIRS :=\
	clib\
	mllib\
	refiner\
	filter\
	library\
	theories/tactic\
	theories/base\
	theories/itt\
	theories/rewrite

.PHONY: all install depend clean

all:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
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
