#!/bin/sh
if [ ! -d "$1" ]; then
   echo '$1' should be a directory!
   exit 1
fi
if [ -z "$LOGNAME" ]; then
   LOGNAME=`whoami`
fi
cd $1
rm -f editor/ml/mp.opt
((
if [ "$2" = "update" -or "$2" = "status" ]; then
   echo "*** svn status ***"
   svn status
   echo ""
fi
unset OMAKEFLAGS
omake VERBOSE=1 -S editor/ml/mp.opt
sleep 10
if [ -f editor/ml/mp.opt ]; then
   TEMP=`mktemp /tmp/check1.XXXXXX`
   TEMP2=`mktemp /tmp/check2.XXXXXX`
   TIME=`mktemp /tmp/mktime.XXXXXX`
   export TERM=dumb
   echo 'set_dfmode "src";; check_all ();;' | \
   /usr/bin/time -f 'Check time: %Uuser %Ssystem %Eelapsed (%PCPU); pagefaults: %Fmajor+%Rminor; %Wswaps' -o $TIME editor/ml/mpopt -batch >$TEMP 2>&1
   cat $TIME
   rm -f $TIME
   gawk -F ' proof [(]| rule boxes,| primitive steps' -f "util/do-check-all.awk" < $TEMP
   gawk '/^Entering / {next}; /^Module: / {MOD=$2}; /^Refiner status:/ { $3 = MOD "/" $3 }; {print $0}' < $TEMP > $TEMP2
   cat $TEMP2
   rm -f $TEMP
   echo ""
   echo "A complete log can be found in $TEMP2 on `hostname -s`"
   echo ""
   echo "Complete list off all rules with ungrounded proofs:"
   grep ungrounded $TEMP2
else
   echo ""
   echo BUILD FAILED!
fi) 2>&1 ) | mail -s "MetaPRL full status check (`hostname -s`, `pwd`, rev `cat editor/ml/svnversion.txt`)" "$LOGNAME"

