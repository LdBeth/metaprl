#
# Refiner parts are in subdirectories
#
DIR := refiner
ROOT := ..

include $(ROOT)/mk/preface

INCLUDE :=

DIRS :=\
	refbase\
	refsig\
	term_gen\
	term_std\
	term_ds\
	rewrite\
	refiner\
	reflib

IDIRS := $(addprefix -I , $(DIRS))

CMALIBS := $(addsuffix /reflib.cma, $(DIRS))
CMXALIBS := $(addsuffix /reflib.cmxa, $(DIRS))

MAINLIB := refiner

INSTALL_LIBS := $(MAINLIB)

.PHONY: lib optlib

all: lib 
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB).cma

opt: optlib 
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB).cmxa 
	@ $(MAKE) $(ROOT)/lib/$(MAINLIB)$(EXT_LIB)

#
# The real Makefile
#
include $(ROOT)/mk/rules

#
# Sub-dependencies
#
include refbase/Files
include refsig/Files
include term_std/Files
include term_gen/Files
include term_ds/Files
include rewrite/Files
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
REWRITE_CMXFILES   := $(addprefix rewrite/,  $(addsuffix .cmx, $(REWRITE_FILES)))
REFINER_CMXFILES   := $(addprefix refiner/,  $(addsuffix .cmx, $(REFINER_FILES)))
REFLIB_CMXFILES    := $(addprefix reflib/,   $(addsuffix .cmx, $(REFLIB_FILES)))
CMXFILES :=\
	$(REFBASE_CMXFILES)\
	$(REFSIG_CMXFILES)\
	$(TERM_GEN_CMXFILES)\
	$(TERM_STD_CMXFILES)\
	$(TERM_DS_CMXFILES)\
	$(REWRITE_CMXFILES)\
	$(REFINER_CMXFILES)\
	$(REFLIB_CMXFILES)

#
# Libraries
#
$(MAINLIB).cma: $(CMALIBS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAINLIB).cma $(CMALIBS)

$(MAINLIB)$(EXT_LIB): $(MAINLIB).cmxa

$(MAINLIB).cmxa: $(CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $(MAINLIB).cmxa $(CMXFILES)

#
# Standard build rules
#
lib:
	@for i in $(DIRS); do\
		if (echo Making refiner/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

optlib:
	@for i in $(DIRS); do\
		if (echo Making refiner/$$i...; $(MAKE) -C $$i opt); then true; else exit 1; fi;\
	done

install::
	@for i in $(DIRS); do\
		if (echo Making refiner/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done
	$(INSTALL) $(MAINLIB).cma $(INSTALLLIB)

clean::
	@for i in $(DIRS); do\
		if (echo Cleaning refiner/$$i...; $(MAKE) -C $$i $@); then true; else exit 1; fi;\
	done

depend::
	@$(RM) Makefile.dep
	@for i in $(DIRS); do\
		if (echo Making refiner/$$i...; cd $$i && $(RM) Makefile.dep); then true; else exit 1; fi;\
	done

