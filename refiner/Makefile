#
# Refiner parts are in subdirectories
#
INCLUDE :=

DIRS :=\
	refbase\
	refsig\
	term_std\
	term_ds\
	refiner\
	reflib

LIBS := $(addsuffix /lib.cma, $(DIRS))

MAIN := refiner

.PHONY: all install depend clean

all:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAIN).cma $(LIBS)

install::
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done
	$(INSTALL) $(MAIN).cma $(NLLIB)

clean::
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

depend::
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; touch Makefile.dep; $(MAKE) $@); then true; else exit 1; fi;\
	done

include ../mk/config
