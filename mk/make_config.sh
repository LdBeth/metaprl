#/bin/sh

if [ ! -d $ENSROOT ]; then
   ENSROOT=undefined
fi
if [ ! -d $OCAMLSRC ]; then
   OCAMLSRC=undefined
fi

/bin/echo "# Main configuration file\n\
#\n\
# Term module to use: ds or std\n\
# If not sure, use ds\n\
#\n\
TERMS=$TERMS\n\
\n\
#\n\
# What representation to use for hypothesis and conclisions lists\n\
# Possible values: Array, Splay (for splay trees)\n\
# If not sure, use Array\n\
#\n\
SEQ_SET=$SEQ_SET\n\
\n\
#\n\
# Refiner verbosity: VERBOSE or SIMPLE\n\
# If not sure, use VERBOSE\n\
#\n\
REFINER=$REFINER\n\
\n\
#\n\
# C compiler\n\
#\n\
CCC=$CCC\n\
\n\
#\n\
# How many compile jobs to start simultaneously\n\
# Recomended value - between #_of_processors and 2 * #_of_processors\n\
# On mojave.cs.cornell.edu and tulare.cs.cornell.edu use 4.
#\n\
MAKE_JOBS=$MAKE_JOBS\n\
\n\
#\n\
# If ENSROOT is defined, it should point\n\
# to the root of the Ensemble source tree\n\
# In this case Ensemble support would be compiled into Meta-PRL\n\
#\n\
ENSROOT=$ENSROOT\n\
\n\
#\n\
# If OCAMLSRC is defined, it should point\n\
# to the root of the OCaml source tree\n\
# In this case Jason's marshaller debugging code
# would be compiled into Meta-PRL\n\
# Do not enable this unless you know what you are doing\!\n\
#\n\
OCAMLSRC=$OCAMLSRC\
"

