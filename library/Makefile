#
# requires /usr/local/gnu/bin version of make.
#

# Libraries
DIR := library
ROOT := ..
INCLUDE := -I .\
	-I $(ROOT)/mllib\
	-I $(ROOT)/refiner/refbase\
	-I $(ROOT)/refiner/refsig\
	-I $(ROOT)/refiner/refiner

OCAMLFLAGS :=
OCAMLC := ocamlc

# Main files
PFILES :=\
	test

# Libraries
MLLIBS :=\
	$(ROOT)/mllib/util\
	$(ROOT)/refiner/refiner\
	library

CMALIBS :=\
	unix.cma

CCLIBS =\
	-cclib $(ROOT)/lib/clib.a\
	-cclib -lunix

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
	ascii_scan\
	db\
	tentfunctor\
	oidtable\
	definition\
	orb\
	library\
	library_type_base\
	library_eval

# Name of main libraris
MAIN = library

INSTALL_LIBS := $(MAIN)

all: lib

#
# Real makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(MAIN).run $(RMLIFILES) $(RCMIFILES) $(RCMAFILES) $(ROOT)/lib/registry.txt
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES) $(ROOT)/lib/registry.txt
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma
	$(INSTALL) $(MAIN).cma registry.txt $(NLLIB)

clean::
	rm -f *.p4* *.pp*

$(ROOT)/lib/registry.txt: registry.txt
	$(LN) $(ROOT)/$(DIR)/registry.txt $@

