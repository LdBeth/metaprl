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

#
# Optimization
#
REFBASE_CMIFILES   := $(addprefix refbase/,  $(addsuffix .cmi, $(REFBASE_FILES)))
REFSIG_CMIFILES    := $(addprefix refsig/,   $(addsuffix .cmi, $(REFSIG_FILES)))
TERM_STD_CMIFILES  := $(addprefix term_std/, $(addsuffix .cmi, $(TERM_STD_FILES)))
TERM_GEN_CMIFILES  := $(addprefix term_gen/, $(addsuffix .cmi, $(TERM_GEN_FILES)))
TERM_DS_CMIFILES   := $(addprefix term_ds/,  $(addsuffix .cmi, $(TERM_DS_FILES)))
REFINER_CMIFILES   := $(addprefix refiner/,  $(addsuffix .cmi, $(REFINER_FILES)))
REFLIB_CMIFILES    := $(addprefix reflib/,   $(addsuffix .cmi, $(REFLIB_FILES)))

REFBASE_CMOFILES   := $(addprefix refbase/,  $(addsuffix .cmo, $(REFBASE_FILES)))
REFSIG_CMOFILES    := $(addprefix refsig/,   $(addsuffix .cmo, $(REFSIG_FILES)))
TERM_STD_CMOFILES  := $(addprefix term_std/, $(addsuffix .cmo, $(TERM_STD_FILES)))
TERM_GEN_CMOFILES  := $(addprefix term_gen/, $(addsuffix .cmo, $(TERM_GEN_FILES)))
TERM_DS_CMOFILES   := $(addprefix term_ds/,  $(addsuffix .cmo, $(TERM_DS_FILES)))
REFINER_CMOFILES   := $(addprefix refiner/,  $(addsuffix .cmo, $(REFINER_FILES)))
REFLIB_CMOFILES    := $(addprefix reflib/,   $(addsuffix .cmo, $(REFLIB_FILES)))

REFBASE_MLIFILES   := $(addprefix refbase/,  $(addsuffix .mli, $(REFBASE_FILES)))
REFSIG_MLIFILES    := $(addprefix refsig/,   $(addsuffix .mli, $(REFSIG_FILES)))
TERM_STD_MLIFILES  := $(addprefix term_std/, $(addsuffix .mli, $(TERM_STD_FILES)))
TERM_GEN_MLIFILES  := $(addprefix term_gen/, $(addsuffix .mli, $(TERM_GEN_FILES)))
TERM_DS_MLIFILES   := $(addprefix term_ds/,  $(addsuffix .mli, $(TERM_DS_FILES)))
REFINER_MLIFILES   := $(addprefix refiner/,  $(addsuffix .mli, $(REFINER_FILES)))
REFLIB_MLIFILES    := $(addprefix reflib/,   $(addsuffix .mli, $(REFLIB_FILES)))
MLIFILES :=\
	$(REFBASE_MLIFILES)\
	$(REFSIG_MLIFILES)\
	$(TERM_STD_MLIFILES)\
	$(TERM_GEN_MLIFILES)\
	$(TERM_DS_MLIFILES)\
	$(REFINER_MLIFILES)\
	$(REFLIB_MLIFILES)

REFBASE_MLFILES    := $(addprefix refbase/,  $(addsuffix .ml, $(REFBASE_FILES)))
REFSIG_MLFILES     := $(addprefix refsig/,   $(addsuffix .ml, $(REFSIG_FILES)))
TERM_STD_MLFILES   := $(addprefix term_std/, $(addsuffix .ml, $(TERM_STD_FILES)))
TERM_GEN_MLFILES   := $(addprefix term_gen/, $(addsuffix .ml, $(TERM_GEN_FILES)))
TERM_DS_MLFILES    := $(addprefix term_ds/,  $(addsuffix .ml, $(TERM_DS_FILES)))
REFINER_MLFILES    := $(addprefix refiner/,  $(addsuffix .ml, $(REFINER_FILES)))
REFLIB_MLFILES     := $(addprefix reflib/,   $(addsuffix .ml, $(REFLIB_FILES)))
MLFILES :=\
	$(REFBASE_MLFILES)\
	$(REFSIG_MLFILES)\
	$(TERM_STD_MLFILES)\
	$(TERM_GEN_MLFILES)\
	$(TERM_DS_MLFILES)\
	$(REFINER_MLFILES)\
	$(REFLIB_MLFILES)

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

refbase/reflib.cma: $(REFBASE_CMIFILES) $(REFBASE_CMOFILES) $(REFBASE_MLIFILES) $(REFBASE_MLOFILES)
	cd refbase; $(MAKE) all

refsig/reflib.cma: $(REFSIG_CMIFILES) $(REFSIG_CMOFILES) $(REFSIG_MLIFILES) $(REFSIG_MLFILES)
	cd refsig; $(MAKE) all

term_gen/reflib.cma: $(TERM_GEN_CMIFILES) $(TERM_GEN_CMOFILES) $(TERM_GEN_MLIFILES) $(TERM_GEN_MLFILES)
	cd term_gen; $(MAKE) all

term_std/reflib.cma: $(TERM_STD_CMIFILES) $(TERM_STD_CMOFILES) $(TERM_STD_MLIFILES) $(TERM_STD_MLFILES)
	cd term_std; $(MAKE) all

term_ds/reflib.cma: $(TERM_DS_CMIFILES) $(TERM_DS_CMOFILES) $(TERM_DS_MLIFILES) $(TERM_DS_MLFILES)
	cd term_ds; $(MAKE) all

refiner/reflib.cma: $(REFINER_CMIFILES) $(REFINER_CMOFILES) $(REFINER_MLIFILES) $(REFINER_MLFILES)
	cd refiner; $(MAKE) all

reflib/reflib.cma: $(REFLIB_CMIFILES) $(REFLIB_CMOFILES) $(REFLIB_MLIFILES) $(REFLIB_MLFILES)
	cd reflib; $(MAKE) all

$(REFBASE_CMIFILES): %.cmi: %.mli
	cd refbase; $(MAKE) all

$(REFBASE_CMOFILES): %.cmo: %.ml
	cd refbase; $(MAKE) all

$(REFSIG_CMIFILES): %.cmi: %.mli
	cd refsig; $(MAKE) all

$(REFSIG_CMOFILES): %.cmo: %.ml
	cd refsig; $(MAKE) all

$(TERM_GEN_CMIFILES): %.cmi: %.mli
	cd term_gen; $(MAKE) all

$(TERM_GEN_CMOFILES): %.cmo: %.ml
	cd term_gen; $(MAKE) all

$(TERM_STD_CMIFILES): %.cmi: %.mli
	cd term_std; $(MAKE) all

$(TERM_STD_CMOFILES): %.cmo: %.ml
	cd term_std; $(MAKE) all

$(TERM_DS_CMIFILES): %.cmi: %.mli
	cd term_ds; $(MAKE) all

$(TERM_DS_CMOFILES): %.cmo: %.ml
	cd term_ds; $(MAKE) all

$(REFINER_CMIFILES): %.cmi: %.mli
	cd refiner; $(MAKE) all

$(REFINER_CMOFILES): %.cmo: %.ml
	cd refiner; $(MAKE) all

$(REFLIB_CMIFILES): %.cmi: %.mli
	cd reflib; $(MAKE) all

$(REFLIB_CMOFILES): %.cmo: %.ml
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
