#/bin/sh

if [ ! -d $ENSROOT ]; then
   ENSROOT=undefined
fi
if [ ! -d $OCAMLSRC ]; then
   OCAMLSRC=undefined
fi

cat << end_of_cat
# Main configuration file
#
# Term module to use: ds or std
# If not sure, use ds
#
TERMS=$TERMS

#
# What representation to use for hypothesis and conclisions lists
# Possible values: Array, Splay (for splay trees)
# If not sure, use Array
#
SEQ_SET=$SEQ_SET

#
# Refiner verbosity: VERBOSE or SIMPLE
# If not sure, use VERBOSE
#
REFINER=$REFINER

#
# C compiler
#
CCC=$CCC

#
# How many compile jobs to start simultaneously
# Recomended value - between #_of_processors and 2 * #_of_processors
# On mojave.cs.cornell.edu and tulare.cs.cornell.edu use 4.
#
MAKE_JOBS=$MAKE_JOBS

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
# Do not enable this unless you know what you are doing\!
#
OCAMLSRC=$OCAMLSRC\
end_of_cat
