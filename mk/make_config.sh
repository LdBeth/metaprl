#/bin/sh

# If you want to add a new variable to this file (mk/make_config.sh),
# make sure you add a default value to mk/preface
# and add this value to the mk/config target in the top-level Makefile

if [ "$ENSROOT"x = x -o ! -d "$ENSROOT" ]; then
   ENSROOT=undefined
fi
if [ "$OCAMLSRC"x = x -o ! -d "$OCAMLSRC" ]; then
   OCAMLSRC=undefined
fi

cat << end_of_cat
# Main MetaPRL configuration file.

# Term module to use: ds or std
# See doc/htmlman/developer-guide/term_modules.html or 
# http://ensemble01.cs.cornell.edu:12000/cvsweb/~checkout~/meta-prl/doc/htmlman/developer-guide/term_modules.html
# for more information.
# If not sure, use ds
#
TERMS=$TERMS

#
# What representation to use for hypothesis and conclusion lists
# Possible values: Array, Splay (for splay trees)
# If not sure, use Array
#
SEQ_SET=$SEQ_SET

#
# Refiner verbosity: VERBOSE or SIMPLE
# See doc/htmlman/developer-guide/refiner_verb_and_simp.html or
# http://ensemble01.cs.cornell.edu:12000/cvsweb/~checkout~/meta-prl/doc/htmlman/developer-guide/refiner_verb_and_simp.html
# for more information.
# If not sure, use VERBOSE
#
REFINER=$REFINER

#
# This is the list of theory directories theory/*
# that you want to compile.  You want to include at least
#    THEORIES = tactic ocaml base
# Include itt if you want to use the Nuprl type theory,
# and add any additional theory directories after that.
#
THEORIES=$THEORIES

#
# Use GNU readline package (available on Linux at least) (yes/no).
#
READLINE=$READLINE

#
# C compiler
#
CCC=$CCC

#
# How many compile jobs to start simultaneously
# Recomended value - between #_of_processors and 2 * #_of_processors
# On mojave.cs.cornell.edu and tulare.cs.cornell.edu use 4.
#
# WARNING: There is a problem with make 3.77 that prevents it
# from understanding our Makefiles correctly. If you have make 3.77
# (run make -v to be sure), use MAKE_JOBS=1
#
MAKE_JOBS=$MAKE_JOBS

#
# Extra make options
#
MAKE_OPTS=$MAKE_OPTS

#
# Whether to compile in various test theories and files (yes/no)
#
TESTS=$TESTS

#
# If ENSROOT is defined, it should point
# to the root of the Ensemble source tree
# In this case Ensemble support would be compiled into Meta-PRL
#
ENSROOT=$ENSROOT

#
# If OCAMLSRC is defined, it should point
# to the root of the OCaml source tree
# In this case Jason's marshaller debugging code
# would be compiled into Meta-PRL
# Do not enable this unless you know what you are doing!
#
OCAMLSRC=$OCAMLSRC

# This file (mk/config) is generated by make using mk/make_config.sh 
# If you want to change anything except for the variable values, 
# edit mk/make_config.sh instead.

end_of_cat
