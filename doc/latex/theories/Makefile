#
# Libraries
#
DIR = doc/latex/theories
ROOT = ../../..
ifndef TEXINPUTS
	TEXINPUTS = ../inputs:.:
else
	TEXINPUTS = ../inputs:$(TEXINPUTS)
endif

include $(ROOT)/mk/preface

.PHONY: doc

all: doc
opt: doc

INPUTS = ../inputs/metaprl.bib\
	../inputs/metaprl.tex\
	all-theories.tex

TEXTHEORIES=\
	base\
	fol\
	itt\
	czf

TEXFILES = $(addsuffix /theory.tex, $(TEXTHEORIES))\
	$(addsuffix /theory-body.tex, $(TEXTHEORIES))

doc:
	+ @ $(MAKE) -C $(ROOT)/editor/ml TEXTHEORIES="$(TEXTHEORIES)" tex
	+ @ $(MAKE) $(ROOT)/doc/ps/theories/theories.pdf

all-theories.pdf: $(INPUTS) $(TEXFILES)
	touch all-theories.ind
	TEXINPUTS=$(TEXINPUTS) pdflatex all-theories
	if [ grep -q \\\citation all-theories.aux ]; then \
   	TEXINPUTS=$(TEXINPUTS) bibtex all-theories; \
		TEXINPUTS=$(TEXINPUTS) pdflatex all-theories; \
		TEXINPUTS=$(TEXINPUTS) pdflatex all-theories; \
	fi
	TEXINPUTS=$(TEXINPUTS) makeindex all-theories
	TEXINPUTS=$(TEXINPUTS) pdflatex all-theories
	cp -a all-theories.pdf $(ROOT)/doc/ps/theories/theories.pdf

$(ROOT)/doc/ps/theories/theories.pdf: all-theories.pdf
	$(CP) all-theories.pdf $@

clean::
	$(RM) *.log *.aux *.idx *.ilg *.ind *.pdf

# Real makefile
include $(ROOT)/mk/rules
