# Libraries
INCLUDE := -I . -I ../../theories/tactic -I $(NLLIB) -I /usr/local/lib/camlp4

# Common files
MLZFILES :=\
	edit_type

# Library files
NLFILES :=\
	edit_type\
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
	shell

MFILES :=\
	editor_init

PFILES :=\
	main

# Name of main libraris
MAIN = nuprl-light

all: $(MAIN).cma $(MAIN).run editor_init.cmo
opt: $(MAIN).cmxa $(MAIN) editor_init.cmx

clean::
	$(RM) -f *.p4* *.pp*

# Real makefile
include ../../mk/config
include Makefile.dep
