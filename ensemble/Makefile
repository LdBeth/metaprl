# Flags
DIR := ensemble
ROOT := ..

INCLUDE :=\
	-I .\
	-I $(ENSROOT)/lib\
	-I $(ROOT)/theories/tactic\
	-I $(ROOT)/theories/ocaml\
	-I $(ROOT)/theories/base\
	-I $(ROOT)/theories/itt\
	-I $(ROOT)/theories/czf\
	-I $(ROOT)/lib

# Library files
PFILES :=\
	remote_refiner\
	nlapp

CMXDEPS =\
	$(CAMLLIB)/unix.cmxa\
        $(CAMLLIB)/nums.cmxa\
	$(ROOT)/lib/util.cmxa\
	$(ROOT)/lib/refiner.cmxa\
	$(ROOT)/lib/library.cmxa\
	$(ROOT)/theories/tactic/tactics.cmxa\
	$(ROOT)/theories/ocaml/ocaml_df.cmxa\
	$(ROOT)/theories/base/base.cmxa\
	$(ROOT)/theories/itt/itt.cmxa\
	$(ROOT)/theories/czf/czf.cmxa\
	$(MAIN).cmxa

CMXLIBS =\
	$(CMXDEPS)

ifeq ($(OSTYPE), win32)
    LIBS_ens =\
	$(ENSROOT)/lib/socket-$(HOSTTYPE)-$(OSTYPE).cma\
	$(ENSROOT)/lib/crypto-$(HOSTTYPE)-$(OSTYPE).cma\
	$(ENSROOT)/lib/libens-$(HOSTTYPE)-$(OSTYPE).cma
else
    LIBS_ens =\
	$(ENSROOT)/lib/socket.cma\
	$(ENSROOT)/lib/crypto.cma\
	$(ENSROOT)/lib/libens.cma
endif

CMADEPS =\
	$(CAMLLIB)/unix.cma\
        $(CAMLLIB)/nums.cma\
	$(ROOT)/lib/util.cma\
	$(ROOT)/lib/refiner.cma\
	$(ROOT)/lib/library.cma\
	$(ROOT)/theories/tactic/tactics.cma\
	$(ROOT)/theories/ocaml/ocaml_df.cma\
	$(ROOT)/theories/base/base.cma\
	$(ROOT)/theories/itt/itt.cma\
	$(ROOT)/theories/czf/czf.cma\
	$(LIBS_ens)

CMALIBS =\
	$(CMADEPS)

CCLIBS =\
	-cclib -lunix\
        -cclib -lnums\
	-cclib $(ROOT)/lib/clib.a\
	-ccopt -L$(ENSROOT)/lib\
	-cclib -lsock-$(HOSTTYPE)\
	-cclib -lcryptoc-$(HOSTTYPE)

# Name of library
MAIN := nlapp

INSTALL_LIBS := $(MAIN)

.PHONY: lib

all: $(MAIN).run
opt: $(MAIN).opt

#
# Actual makefile
#
include $(ROOT)/mk/config
include Makefile.dep

#
# Rules
#
lib: $(RMLIFILES) $(RCMIFILES) $(RCMAFILES)
opt: $(RMLIFILES) $(RCMIFILES) $(RCMXAFILES) 
	@ $(MAKE) $(RAFILES)

install:: $(MAIN).cma $(CMIFILES)
	$(INSTALL) $(MAIN).cma $(MLIFILES) $(CMIFILES) $(NLLIB)

$(NLAPP).run: $(CMOBJS)
	rm -f $@
	$(OCAMLC) -o $@ -custom -linkall $(CMOBJS) $(CCLIBS)

clean::
	$(RM) -f *.p4* *.pp*
