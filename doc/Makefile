# In order to use this Makefile, you need to have HTMLDOC utility
# HTMLDOC utility can be downloaded from http://www.easysw.com/~mike/htmldoc/
# HTMLDOC RPMs can be dowloaded from http://www.cs.cornell.edu/nogin/RPM/htmldoc.html

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
	mp-frame\
	mp-links\
	mp-install\
	mp-people\
	license

USER-GUIDE =\
	mp-user-guide\
	mp-terms\
	mp-dform\
	mp-axiom\
	mp-rewrite\
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
	mp-term\
	mp-rewrite\
	mp-refine\
	mp-filter\
	mp-ensemble\
	mp-chaining\
	mp-tactic\
	mp-tacticals\
	mp-conversionals.html\
	mp-ocaml\
	mp-base\
	mp-editor-imp\
	mp-itt

W-FILES = $(addprefix htmlman/, $(addsuffix .html, $(WELCOME)))
UG-FILES = $(addprefix htmlman/user-guide/, $(addsuffix .html, $(USER-GUIDE)))
T-FILES = $(addprefix htmlman/tutorial/, $(addsuffix .html, $(TUTORIAL)))
S-FILES = $(addprefix htmlman/system/, $(addsuffix .html, $(SYSTEM)))

GENERATED-FILES =\
	introduction\
	user-guide\
	tutorial\
	system

DOCFILES = $(addsuffix .ps, $(GENERATED-FILES)) $(addsuffix .pdf, $(GENERATED-FILES))

all: $(DOCFILES)

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

clean::
	$(RM) $(DOCFILES)

check::
	env WEBLINTRC=weblintrc weblint .
