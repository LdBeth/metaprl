#
# requires /usr/local/gnu/bin version of make.
#

# Libraries
REF_ROOT := ..
INCLUDE := -I . -I $(REF_ROOT)/mllib -I $(REF_ROOT)/refiner
OCAMLFLAGS := -g
OCAMLC := ocamlc

# Main files
PFILES :=\
	test

# Libraries
MLLIBS :=\
	$(REF_ROOT)/mllib/util\
	$(REF_ROOT)/refiner/refiner\
	library

CMALIBS :=\
	nums.cma\
	unix.cma

CCLIBS = -cclib -lunix -cclib -lnums

# Library files
LMFILES :=\
	utils\
	int32\
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

all: $(MAIN).run $(MAIN).cma
opt: $(MAIN).cmxa

install:: $(MAIN).cma
	$(INSTALL) $(MAIN).cma $(NLLIB)

clean::
	rm -f *.p4* *.pp*

# Add dependency for parser
$(addsuffix .cmiz, $(MFILES)) $(addsuffix .cmoz, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma
$(addsuffix .p4i, $(MFILES)) $(addsuffix .p4, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma

# Real makefile
include $(REF_ROOT)/mk/config
include Makefile.dep
