#!/bin/sh
TEMP=`mktemp /tmp/mkstatus2.XXXXXX`
TIME=`mktemp /tmp/mktime.XXXXXX`
export TERM=dumb
echo 'set_dfmode "src";; check_all ();;' | \
/usr/bin/time -f 'Check time: %Uuser %Ssystem %Eelapsed (%PCPU); pagefaults: %Fmajor+%Rminor; %Wswaps' -o $TIME editor/ml/mpopt > /dev/null 2>$TEMP
cat $TIME
rm -f $TIME
gawk -F ' proof [(]| rule boxes,| primitive steps' -f "util/do-check-all.awk" < $TEMP
gawk -F"[[:blank:]\`']+" '/Module: / {MOD=$2}; /^Refiner status:/ { $3 = MOD "/" $3 }; {print $0}' < $TEMP
