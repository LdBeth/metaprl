#
# Libraries
#
DIR = doc/latex/theories
ROOT = ../../..
ifndef TEXINPUTS
	TEXINPUTS := ../inputs:.:
else
	TEXINPUTS := ../inputs:$(TEXINPUTS)
endif

include $(ROOT)/mk/preface

NAME=all-theories

.PHONY: doc

all: doc
opt: doc

INPUTS = ../inputs/metaprl.bib\
	../inputs/metaprl.tex\
	$(NAME).tex

TEXTHEORIES=\
	base\
	fol\
	itt\
	czf\
	mc

TEXFILES = $(addsuffix /theory.tex, $(TEXTHEORIES))\
	$(addsuffix /theory-body.tex, $(TEXTHEORIES))\
	all-bodies.tex

doc:
	+ @ $(MAKE) -C $(ROOT)/editor/ml TEXTHEORIES="$(TEXTHEORIES)" tex
	+ @ $(MAKE) $(ROOT)/doc/ps/theories/theories.pdf

ALLBODIES=$(addprefix \input{, $(addsuffix /theory-body.tex}, $(TEXTHEORIES)))

all-bodies.tex: Makefile
	echo '$(ALLBODIES)' > $@

$(NAME).aux: $(INPUTS) $(TEXFILES)
	TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME)
	if grep -q \\\citation $(NAME).aux; then \
		TEXINPUTS=$(TEXINPUTS) bibtex $(NAME); \
		TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME); \
		TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME); \
	fi

$(NAME).ind: $(INPUTS) $(TEXFILES)
	+ @ $(MAKE) $(NAME).aux
	TEXINPUTS=$(TEXINPUTS) makeindex $(NAME)

$(NAME).pdf: $(INPUTS) $(TEXFILES) $(NAME).ind
	TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME)
	echo $(NAME).pdf : `grep -a '/Type /Page$$' $(NAME).pdf | wc -l` pages 1>&2

$(NAME).dvi: $(INPUTS) $(TEXFILES) $(NAME).ind
	TEXINPUTS=$(TEXINPUTS) latex $(NAME)

$(ROOT)/doc/ps/theories/theories.pdf: $(NAME).pdf
	$(CP) $(NAME).pdf $@

clean::
	$(RM) *.log *.aux *.idx *.ilg *.ind *.pdf *.ps *.dvi *.blg *.bbl *.out
	$(RM) */theory-body.tex */theory.tex all-bodies.tex

# Real makefile
include $(ROOT)/mk/rules
