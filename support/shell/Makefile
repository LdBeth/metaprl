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
	package_sig\
	shell_sig\
	shell_p4_sig\

# Library files
MPFILES =\
	mux_channel\
	display_term\
	proof_edit\
	package_sig\
	package_info\
	shell_sig\
	shell_rewrite\
	shell_rule\
	shell_tex\
	shell_package\
	shell_root\
	recursive_lock\
	mptop\
	shell_http\
	shell_state\
	shell_p4_sig\
	shell\

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
