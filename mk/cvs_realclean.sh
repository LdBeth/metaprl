#!/bin/sh
$MAKE -s clean

find . \( -name \*.prlb -or -name '.#*' \) -exec rm -f "{}" \;

function clean () {
   if [ -f "$1/CVS/Entries" ]; then
      for f in `ls -1A $1`; do
         if [ -d "$1/$f" ]; then
            CVS=`grep "^D/$f/" "$1/CVS/Entries"`
            if [ -n "$CVS" ]; then
               clean "$1/$f"
            else
               if [ "$f" != "CVS" ]; then
                  rm -ir -- "$1/$f"
               fi
            fi
         else
            CVS=`grep "^/$f/" "$1/CVS/Entries"`
            if [ -z "$CVS" ]; then
               rm -i -- "$1/$f"
            fi
         fi
      done
   else
      echo "No CVS information for directory $1"
      exit 1
   fi
}

clean .

