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

DEFTEXTHEORIES=\
	base\
	fol\
	itt\
	czf\
	fir

.PHONY: doc all-doc theories all-theories ocaml ocaml-book compile m-paper mtr m-paper-tr

all: all-doc
opt: all-doc
all-doc: all-theories ocaml-book m-paper

theories all-theories:
	+ @ $(MAKE) NAME=all-theories GOAL=theories TEXTHEORIES="$(DEFTEXTHEORIES)" doc

ocaml ocaml-book:
	+ @ $(MAKE) NAME=book2 GOAL=ocaml-book TEXTHEORIES=ocaml_doc doc

compile m-paper:
	+ @ $(MAKE) NAME=m-paper GOAL=m-paper TEXTHEORIES=experimental/compile MOREINPUTS="../inputs/sig-alternate.cls" doc

mtr m-paper-tr:
	+ @ $(MAKE) NAME=m-paper-tr GOAL=m-paper-tr TEXTHEORIES=experimental/compile doc

INPUTS = $(MOREINPUTS)\
	../inputs/rc.bib\
	../inputs/metaprl.tex\
	$(NAME).tex

TEXFILES = $(addsuffix /theory.tex, $(TEXTHEORIES))\
	$(addsuffix /theory-body.tex, $(TEXTHEORIES))\

doc:
	+ @ $(MAKE) -C $(ROOT)/editor/ml TEXTHEORIES="$(TEXTHEORIES)" tex
	+ @ $(MAKE) NAME=$(NAME) GOAL=$(GOAL) TEXTHEORIES="$(TEXTHEORIES)" $(ROOT)/doc/ps/theories/$(GOAL).ps
	+ @ $(MAKE) NAME=$(NAME) GOAL=$(GOAL) TEXTHEORIES="$(TEXTHEORIES)" $(ROOT)/doc/ps/theories/$(GOAL).pdf

ALLBODIES=$(addprefix \input{, $(addsuffix /theory-body.tex}, $(TEXTHEORIES)))

all-bodies.tex: Makefile
	echo '$(ALLBODIES)' > $@

all-theories.aux: all-bodies.tex

$(NAME).aux: $(INPUTS) $(TEXFILES)
	TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME)
	if grep -q \\\citation $(NAME).aux; then \
		TEXINPUTS=$(TEXINPUTS) bibtex $(NAME); \
		TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME); \
		TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME); \
	fi

$(NAME).ind: $(INPUTS) $(TEXFILES)
	+ @ $(MAKE) NAME=$(NAME) TEXTHEORIES="$(TEXTHEORIES)" $(NAME).aux
	TEXINPUTS=$(TEXINPUTS) makeindex $(NAME)

#$(NAME).pdf: $(INPUTS) $(TEXFILES) $(NAME).ind
#	TEXINPUTS=$(TEXINPUTS) pdflatex $(NAME)
$(NAME).pdf: $(INPUTS) $(TEXFILES) $(NAME).dvi
	dvipdfm -p letter -o $@ $(NAME).dvi
	echo $(NAME).pdf : `grep -a '/Type */Page$$' $(NAME).pdf | wc -l` pages 1>&2

$(NAME).ps: $(INPUTS) $(TEXFILES) $(NAME).dvi
	dvips -t letter -o $@ $(NAME).dvi

$(NAME).dvi: $(INPUTS) $(TEXFILES) $(NAME).ind
	TEXINPUTS=$(TEXINPUTS) latex $(NAME)

$(ROOT)/doc/ps/theories/$(GOAL).pdf: $(NAME).pdf
	$(CP) $(NAME).pdf $@

$(ROOT)/doc/ps/theories/$(GOAL).ps: $(NAME).ps
	$(CP) $(NAME).ps $@

clean::
	$(RM) *.log *.aux *.idx *.ilg *.ind *.pdf *.ps *.dvi *.blg *.bbl *.out *.toc
	$(RM) */theory-body.tex */theory.tex all-bodies.tex
	$(RM) */*/theory-body.tex */*/theory.tex */*/tr-body.tex */*/tr.tex

# Real makefile
include $(ROOT)/mk/rules
