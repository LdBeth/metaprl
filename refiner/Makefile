#
# Refiner parts are in subdirectories
#
ROOT := ..

INCLUDE :=

DIRS :=\
	refbase\
	refsig\
	term_std\
	term_gen\
	term_ds\
	refiner\
	reflib

IDIRS := $(addprefix -I , $(DIRS))

CMALIBS := $(addsuffix /reflib.cma, $(DIRS))
CMXALIBS := $(addsuffix /reflib.cmxa, $(DIRS))

MAINLIB := refiner

INSTALL_LIBS := $(MAINLIB)

.PHONY: lib optlib

all: $(ROOT)/lib/$(MAINLIB).cma

opt: $(ROOT)/lib/$(MAINLIB).cmxa $(ROOT)/lib/$(MAINLIB).a

#
# The real Makefile
#
include $(ROOT)/mk/config

#
# Sub-dependencies
#
include refbase/Files
include refsig/Files
include term_std/Files
include term_gen/Files
include term_ds/Files
include refiner/Files
include reflib/Files

#
# Optimization
#
REFBASE_CMXFILES   := $(addprefix refbase/,  $(addsuffix .cmx, $(REFBASE_FILES)))
REFSIG_CMXFILES    := $(addprefix refsig/,   $(addsuffix .cmx, $(REFSIG_FILES)))
TERM_STD_CMXFILES  := $(addprefix term_std/, $(addsuffix .cmx, $(TERM_STD_FILES)))
TERM_GEN_CMXFILES  := $(addprefix term_gen/, $(addsuffix .cmx, $(TERM_GEN_FILES)))
TERM_DS_CMXFILES   := $(addprefix term_ds/,  $(addsuffix .cmx, $(TERM_DS_FILES)))
REFINER_CMXFILES   := $(addprefix refiner/,  $(addsuffix .cmx, $(REFINER_FILES)))
REFLIB_CMXFILES    := $(addprefix reflib/,   $(addsuffix .cmx, $(REFLIB_FILES)))
CMXFILES :=\
	$(REFBASE_CMXFILES)\
	$(REFSIG_CMXFILES)\
	$(TERM_STD_CMXFILES)\
	$(TERM_GEN_CMXFILES)\
	$(TERM_DS_CMXFILES)\
	$(REFINER_CMXFILES)\
	$(REFLIB_CMXFILES)

#
# Libraries
#
$(MAINLIB).cma: $(CMALIBS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAINLIB).cma $(CMALIBS)

$(MAINLIB).cmxa $(MAINLIB).a: $(CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $(MAINLIB).cmxa $(CMXFILES)

#
# Standard build rules
#
$(CMALIBS): lib
$(CMXFILES): optlib

#
# Standard build rules
#
lib:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

optlib:
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) opt); then true; else exit 1; fi;\
	done

install::
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done
	$(INSTALL) $(MAINLIB).cma $(INSTALLLIB)

clean::
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; $(MAKE) $@); then true; else exit 1; fi;\
	done

depend::
	rm -f Makefile.tmp
	touch Makefile.tmp
	$(ROOT)/bin/ocamldep $(IDIRS) -I ../mllib $(MLFILES) $(MLIFILES) >> Makefile.tmp
	mv Makefile.tmp Makefile.dep
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; touch Makefile.dep; $(MAKE) $@); then true; else exit 1; fi;\
	done

#
# Dependencies
#
include Makefile.dep
