# Flags
DIR = refiner/refiner
ROOT = ../..
INCLUDE := -I ../../mllib -I ../refbase -I ../refsig -I ../term_std -I ../term_gen -I ../term_ds -I ../rewrite

# Library files
include Files
LMFILES := $(REFINER_FILES)

# Name of library
MAIN := reflib

# Flags
CCLIBS := -cclib -lnums

# C preprocessor
OCAMLFLAGS = -pp "$(CPP) $(INCLUDE)"

all: lib

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Suffix for the Refiner module to use.
# Possible values: ds_verb, ds_simp, std_verb, std_simp
# 
# The system looks for suffix to use:
#  1) In .refiner file (if it's non-empty)
#  2) In MP_REFINER environment variable (if set)
#  3) Prints warning and uses "ds_verb"
#

.refiner:
	touch .refiner

refiner.ml: .refiner
	@REF=`cat .refiner`;\
	if [ -n "$$REF" ]; then\
		echo "| .refiner defines Refiner as Refiner_"$$REF;\
	else\
		if [ -n "$$MP_REFINER" ]; then\
			REF="$$MP_REFINER";\
	       		echo "| MP_REFINER environment variable defines Refiner as Refiner_"$$REF;\
	   	else\
			REF="ds";\
			echo "Warning: MP_REFINER is undefined, using Refiner_"$$REF;\
		fi;\
	fi;\
	echo "module Refiner = Refiner_"$$REF".Refiner" > refiner.ml

#
# Rules
#
lib: $(MAIN).cma $(RMLIFILES) $(RCMIFILES)
opt: $(MAIN).cmxa $(RMLIFILES) $(RCMIFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MLIFILES) $(CMIFILES) $(MPLIB)

clean::
	rm -f refiner.ml

