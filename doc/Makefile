# In order to use this Makefile, you need to have HTMLDOC utility
# HTMLDOC utility can be downloaded from http://www.easysw.com/~mike/htmldoc/
# HTMLDOC RPMs can be dowloaded from http://www.cs.cornell.edu/nogin/RPM/htmldoc.html
# You should probably use htmldoc >= 1.8.3

# The "check" target uses weblint utility.
# weblint can be downloaded from http://www.cre.canon.co.uk/~neilb/weblint/
# Weblint RPMs can be downloaded from http://www.cs.cornell.edu/nogin/RPM/weblint.html
# You would probably need weblint >= 1.9

HTMLDOC = htmldoc
OPTS = --no-title --size letter --bodycolor white
GEN-PS = $(HTMLDOC) -t ps2 $(OPTS)
GEN-PDF = $(HTMLDOC) -t pdf $(OPTS)
RM = rm -f

WELCOME =\
	mp\
	mp-links\
	mp-install\
	mp-people\
	license

USER-GUIDE =\
	mp-user-guide\
	mp-terms\
	mp-dform\
	mp-font\
	mp-rewrite\
	mp-axiom\
	mp-modules\
	mp-editor

TUTORIAL =\
	mp-tutorial\
	mp-getting-started\
	mp-type\
	mp-base\
	mp-base-auto\
	mp-simple\
	mp-not\
	mp-all\
	mp-struct\
	mp-ctheory\
	mp-class\
	mp-theory

SYSTEM =\
	mp-system\
	mp-refiner\
	mp-terms\
	mp-rewrite\
	mp-refine\
	mp-filter\
	mp-ensemble\
	mp-chaining\
	mp-tactic\
	mp-tacticals\
	mp-conversionals\
	mp-ocaml\
	mp-base\
	mp-base-syntax\
	mp-auto-tactic\
	mp-d-tactic\
	mp-type-inf-rsrc\
	mp-base-cache\
	mp-editor-imp\
	mp-itt

DEVELOPER-GUIDE =\
	mp-developer-guide\
	indentation_and_spacing\
	exceptions\
	debugging\
	refiner_verb_and_simp\
	term_modules\
	term_ds_types\
	term_ds_free_vars\
	term_ds_safety\
	profiling

W-FILES = $(addprefix htmlman/, $(addsuffix .html, $(WELCOME)))
UG-FILES = $(addprefix htmlman/user-guide/, $(addsuffix .html, $(USER-GUIDE)))
T-FILES = $(addprefix htmlman/tutorial/, $(addsuffix .html, $(TUTORIAL)))
S-FILES = $(addprefix htmlman/system/, $(addsuffix .html, $(SYSTEM)))
D-FILES = $(addprefix htmlman/developer-guide/, $(addsuffix .html, $(DEVELOPER-GUIDE)))

GENERATED-FILES =\
	introduction\
	user-guide\
	system\
	developer-guide\
	#tutorial

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

tutorial.ps: $(T-FILES)
	$(GEN-PS) $(T-FILES) -f $@

tutorial.pdf: $(T-FILES)
	$(GEN-PDF) $(T-FILES) -f $@

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
