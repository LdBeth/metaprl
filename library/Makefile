#
# requires /usr/local/gnu/bin version of make.
#

# Libraries
REF_ROOT := ..
INCLUDE := -I . -I $(REF_ROOT)/mllib -I $(REF_ROOT)/refiner
OCAMLFLAGS := -g
#OCAMLFLAGS := -g -root $(REF_ROOT)
OCAMLC := ocamlc
#OCAMLC := $(REF_ROOT)/filter/prlcomp.run
#CAMLP4 := $(REF_ROOT)/filter/prlcomp.run -E

# Main files
PFILES :=\
	test

# Libraries
MLLIBS :=\
	$(REF_ROOT)/mllib/util\
	$(REF_ROOT)/refiner/refiner\
	library

CMALIBS :=\
	unix.cma

CCLIBS = -cclib -lunix

# Library files
LMFILES :=\
	utils\
	bigInt\
	nuprl5\
	registry\
	mathBus\
	mbterm\
	basic\
	socketIo\
	link\
	db\
	tentfunctor\
	oidtable\
	definition\
	orb\
	library\
	library_type_base

# Name of main libraris
MAIN = library

all: $(MAIN).cma $(MAIN).run
opt: $(MAIN).cmxa

clean::
	del -f *.p4* *.pp*

# Add dependency for parser
$(addsuffix .cmiz, $(MFILES)) $(addsuffix .cmoz, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma
$(addsuffix .p4i, $(MFILES)) $(addsuffix .p4, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma

# Real makefile
include $(REF_ROOT)/mk/config
include Makefile.dep
