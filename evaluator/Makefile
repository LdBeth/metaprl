#
# Base theories.
#
DIR := evaluator
ROOT := ..
INCLUDE := -I $(ROOT)/refiner/term_ds/lib

# Library files
LMFILES :=\
	evaluator

# Name of main libraris
MAIN = evaluator

all: $(MAIN).cma
opt: $(MAIN).cmxa
install:: all

clean::
	$(RM) *.p4* *.pp*

# Real makefile
include $(ROOT)/mk/config
include Makefile.dep
