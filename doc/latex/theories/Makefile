#
# Libraries
#
DIR := doc/latex/theories
ROOT := ../../..

include $(ROOT)/mk/preface

all: doc
opt: doc

doc:	metaprl.bib
	(cd $(ROOT)/editor/ml; make tex)
	pdflatex all-theories
	bibtex all-theories
	pdflatex all-theories
	pdflatex all-theories
	makeindex all-theories
	pdflatex all-theories
	cp all-theories.pdf $(ROOT)/doc/ps/theories/theories.pdf

metaprl.bib: ../inputs/metaprl.bib
	$(LN) ../inputs/metaprl.bib .

clean::
	$(RM) *.log *.aux *.idx *.ilg *.ind *.pdf

# Real makefile
include $(ROOT)/mk/rules
