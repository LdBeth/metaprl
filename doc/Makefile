# In order to use this Makefile, you need to have HTMLDOC utility
# HTMLDOC utility can be downloaded from http://www.easysw.com/~mike/htmldoc/
# HTMLDOC RPMs can be dowloaded from http://www.cs.cornell.edu/nogin/RPM/htmldoc.html

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

W-FILES = $(addprefix htmlman/, $(addsuffix .html, $(WELCOME)))
UG-FILES = $(addprefix htmlman/user-guide/, $(addsuffix .html, $(USER-GUIDE)))
T-FILES = $(addprefix htmlman/tutorial/, $(addsuffix .html, $(TUTORIAL)))

DOCFILES = introduction.ps introduction.pdf user-guide.ps user-guide.pdf tutorial.ps tutorial.pdf

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

clean::
	$(RM) $(DOCFILES)
