#/bin/sh
/bin/echo "# Main configuration file\n\
#\n\
# Term module to use: ds or std\n\
# If not sure, use ds\n\
#\n\
TERMS=$TERMS \n\
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
# How many compile jobs to start simultaneously\n\
# Recomended value - between #_of_processors and 2 * #_of_processors\n\
# On mojave.cs.cornell.edu and tulare.cs.cornell.edu use 4.
#\n\
MAKE_JOBS=$MAKE_JOBS\n\
"

