# Libraries
REF_ROOT := ../..
INCLUDE := -I . -I $(REF_ROOT)/refiner -I $(REF_ROOT)/theories/tactic -I $(REF_ROOT)/theories/base -I $(REF_ROOT)/theories/itt -I /usr/local/lib/camlp4
OCAMLFLAGS := -g -root $(REF_ROOT)
OCAMLC := $(REF_ROOT)/filter/prlcomp.run
CAMLP4 := $(REF_ROOT)/filter/prlcomp.run -E -root $(REF_ROOT)

# Common files
MLZFILES :=\
	edit_type

# Library files
LMFILES :=\
	file_util\
	edit_type\
	imp_dag\
	proof_step\
	proof\
	proof_edit\
	package_info\
	package_util\
	package_graph\
	package_io\
	display_package\
	display_package_graph\
	display_theory_item\
	proof_edit\
	shell

MFILES :=\
	editor_init

PFILES :=\
	main

# Name of main libraris
MAIN = tactics

all: $(MAIN).cma $(MAIN).run editor_init.cmo
opt: $(MAIN).cmxa $(MAIN) editor_init.cmx

clean::
	del -f *.p4* *.pp*

# Add dependency for parser
$(addsuffix .cmiz, $(MFILES)) $(addsuffix .cmoz, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma
$(addsuffix .p4i, $(MFILES)) $(addsuffix .p4, $(MFILES)): $(REF_ROOT)/filter/prlcomp.cma

# Real makefile
include ../../mk/config
include Makefile.dep
