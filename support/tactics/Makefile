#
# Don't need any libraries.
#
DIR := support/tactics
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I . -I ../display -I ../shell -I $(CAMLP4LIB)
OCAMLDEPFLAGS := -prl
PRLCFLAGS := -v

# Common files
MLZFILES =\

# Library files
MPFILES =\
	tactic_cache\
	var\
	top_tacticals\
	top_conversionals\

# Name of main libraris
MAIN = theory

all: $(MAIN).cma
opt: $(MAIN).cmxa
install:: all

clean::
	$(RM) -f *.p4* *.pp*

# Real makefile
include $(ROOT)/mk/rules
include Makefile.dep
