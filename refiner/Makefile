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

CMALIBS := $(addsuffix /reflib.cma, $(DIRS))
CMXALIBS := $(addsuffix /reflib.cmxa, $(DIRS))

MAINLIB := refiner

INSTALL_LIBS := $(MAINLIB)

.PHONY: optlib

all: lib

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

REFBASE_MLFILES := $(addprefix refbase/, $(addsuffix .ml, $(REFBASE_FILES))\
					 $(addsuffix .mli, $(REFBASE_FILES)))

REFSIG_MLFILES := $(addprefix refsig/, $(addsuffix .ml, $(REFSIG_FILES))\
				       $(addsuffix .mli, $(REFSIG_FILES)))

TERM_STD_MLFILES := $(addprefix term_std/, $(addsuffix .ml, $(TERM_STD_FILES))\
					   $(addsuffix .mli, $(TERM_STD_FILES)))

TERM_GEN_MLFILES := $(addprefix term_gen/, $(addsuffix .ml, $(TERM_GEN_FILES))\
					   $(addsuffix .mli, $(TERM_GEN_FILES)))

TERM_DS_MLFILES := $(addprefix term_ds/, $(addsuffix .ml, $(TERM_DS_FILES))\
					 $(addsuffix .mli, $(TERM_DS_FILES)))

REFINER_MLFILES := $(addprefix refiner/, $(addsuffix .ml, $(REFINER_FILES))\
					 $(addsuffix .mli, $(REFINER_FILES)))\
			$(TERM_STD_MLFILES) $(TERM_GEN_MLFILES) $(TERM_DS_MLFILES)

REFLIB_MLFILES := $(addprefix reflib/,  $(addsuffix .ml, $(REFLIB_FILES))\
					$(addsuffix .mli, $(REFLIB_FILES)))

#
# Optimization
#
REFBASE_CMXFILES := $(addprefix refbase/, $(addsuffix .cmx, $(REFBASE_FILES)))
REFSIG_CMXFILES := $(addprefix refsig/, $(addsuffix .cmx, $(REFSIG_FILES)))
TERM_STD_CMXFILES := $(addprefix term_std/, $(addsuffix .cmx, $(TERM_STD_FILES)))
TERM_GEN_CMXFILES := $(addprefix term_gen/, $(addsuffix .cmx, $(TERM_GEN_FILES)))
TERM_DS_CMXFILES := $(addprefix term_ds/, $(addsuffix .cmx, $(TERM_DS_FILES)))
REFINER_CMXFILES := $(addprefix refiner/, $(addsuffix .cmx, $(REFINER_FILES)))
REFLIB_CMXFILES := $(addprefix reflib/,  $(addsuffix .cmx, $(REFLIB_FILES)))
CMXFILES :=\
	$(REFBASE_CMXFILES)\
	$(REFSIG_CMXFILES)\
	$(TERM_STD_CMXFILES)\
	$(TERM_GEN_CMXFILES)\
	$(TERM_DS_CMXFILES)\
	$(REFINER_CMXFILES)\
	$(REFLIB_CMXFILES)

refbase/reflib.cma: $(REFBASE_MLFILES)
	cd refbase; $(MAKE) all

refsig/reflib.cma: $(REFSIG_MLFILES)
	cd refsig; $(MAKE) all

term_gen/reflib.cma: $(TERM_GEN_MLFILES)
	cd term_gen; $(MAKE) all

term_std/reflib.cma: $(TERM_STD_MLFILES)
	cd term_std; $(MAKE) all

term_ds/reflib.cma: $(TERM_DS_MLFILES)
	cd term_ds; $(MAKE) all

refiner/reflib.cma: $(REFINER_MLFILES)
	cd refiner; $(MAKE) all

reflib/reflib.cma: $(REFLIB_MLFILES)
	cd reflib; $(MAKE) all

#
# Libraries
#
$(MAINLIB).cma: $(CMALIBS)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $(MAINLIB).cma $(CMALIBS)

$(MAINLIB).cmxa: $(CMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a -o $(MAINLIB).cmxa $(CMXFILES)

#
# Standard build rules
#
lib: $(RCMAFILES)

opt: $(RCMXAFILES)

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
	@for i in $(DIRS); do\
		if (echo Making $$i...; cd $$i; touch Makefile.dep; $(MAKE) $@); then true; else exit 1; fi;\
	done
