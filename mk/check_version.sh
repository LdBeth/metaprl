#!/bin/sh
#
# Check the program version to make sure it is compatible with MetaPRL
# Usage: check_version.sh <program_name> <extra arguments> <make variable name> <output version string without versions>
# VERSIONS environment variable - space-separated list of valid versions
VERSION=`$1 $2 -v 2>&1 | head -n1`
OK=no
for i in $VERSIONS; do
   if [ "`echo "$VERSION"| tr " " _`" = "`echo "$4 $i"| tr " " _`" ]; then
      exit 0
   fi
done

cat << END_ECHO
ERROR: Wrong CAML version!

"$1 $2 -v" produced the following version string:
$VERSION

You are trying to compile the version of MetaPRL that is compatible
with following Caml versions: $VERSIONS
If you believe that this list of versions is incorrect, edit
the CAML_VERSIONS variable in mk/preface file.

If you want to try to compile anyway, set your environment variable $3
to point to the location of $1 program
END_ECHO
exit 1
