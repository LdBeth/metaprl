# In order to use this Makefile, you need to have HTMLDOC utility
# HTMLDOC utility can be downloaded from http://www.easysw.com/~mike/htmldoc/
# HTMLDOC RPMs can be dowloaded from http://www.cs.cornell.edu/nogin/RPM/htmldoc.html
# You should probably use htmldoc >= 1.8.3

# The "check" target uses weblint utility.
# weblint can be downloaded from http://www.cre.canon.co.uk/~neilb/weblint/
# Weblint RPMs can be downloaded from http://www.cs.cornell.edu/nogin/RPM/weblint.html
# You would probably need weblint >= 1.9

HTMLDOC = htmldoc
OPTS = --no-title --size letter --bodycolor white --toclevel 4
GEN-PS = $(HTMLDOC) -t ps2 $(OPTS)
GEN-PDF = $(HTMLDOC) -t pdf $(OPTS)
RM = rm -f

include Files

W-FILES = $(addprefix htmlman/, $(addsuffix .html, $(WELCOME)))
UG-FILES = $(addprefix htmlman/user-guide/, $(addsuffix .html, $(USER-GUIDE)))
S-FILES = $(addprefix htmlman/system/, $(addsuffix .html, $(SYSTEM)))
D-FILES = $(addprefix htmlman/developer-guide/, $(addsuffix .html, $(DEVELOPER-GUIDE)))

GENERATED-FILES =\
	introduction\
	user-guide\
	system\
	developer-guide\

DOCFILES = $(addsuffix .ps, $(GENERATED-FILES)) $(addsuffix .pdf, $(GENERATED-FILES))

.PHONY: all opt latex clean check ocaml-book

all: html-docs latex ocaml-book
opt: all
html-docs: $(DOCFILES)

introduction.ps: $(W-FILES)
	$(GEN-PS) $(W-FILES) -f $@

introduction.pdf: $(W-FILES)
	$(GEN-PDF) $(W-FILES) -f $@

user-guide.ps: $(UG-FILES)
	$(GEN-PS) $(UG-FILES) -f $@

user-guide.pdf: $(UG-FILES)
	$(GEN-PDF) $(UG-FILES) -f $@

system.ps: $(S-FILES)
	$(GEN-PS) $(S-FILES) -f $@

system.pdf: $(S-FILES)
	$(GEN-PDF) $(S-FILES) -f $@

developer-guide.ps: $(D-FILES)
	$(GEN-PS) $(D-FILES) -f $@

developer-guide.pdf: $(D-FILES)
	$(GEN-PDF) $(D-FILES) -f $@

latex::
	+ @if [ -f ../editor/ml/mp.opt ]; then $(MAKE) -C .. opt; else $(MAKE) -C .. all; fi
	+ @ $(MAKE) -C latex/theories theories

ocaml-book::
	+ @if [ -f ../editor/ml/mp.opt ]; then $(MAKE) -C .. opt; else $(MAKE) -C .. all; fi
	+ @ $(MAKE) -C latex/theories ocaml

clean::
	$(RM) $(DOCFILES) *.log *.dvi *.aux *~ ps/theories/*.ps ps/theories/*.pdf
	+ @ $(MAKE) -C latex/theories clean

check::
	env WEBLINTRC=weblintrc weblint .
