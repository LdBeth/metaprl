#
# Don't need any libraries.
#
DIR := support/shell
ROOT := ../..

include $(ROOT)/mk/preface

INCLUDE := -I . -I ../display -I $(CAMLP4LIB)
OCAMLDEPFLAGS := -prl
PRLCFLAGS := -v

# Common files
MLZFILES =\

# Library files
MPFILES =\
	mptop\

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
