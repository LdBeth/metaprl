#!/bin/sh
TEMP=`mktemp /tmp/mkstatus2.XXXXXX`
TIME=`mktemp /tmp/mktime.XXXXXX`
echo 'set_dfmode "src";; status_all ();;' | \
/usr/bin/time -f 'Expand time: %Uuser %Ssystem %Eelapsed (%PCPU); pagefaults: %Fmajor+%Rminor; %Wswaps' -o $TIME editor/ml/mpopt > /dev/null 2>$TEMP
echo -n "Expand time: "
cat $TIME
rm -f $TIME
gawk -F ' proof [(]| rule boxes,| primitive steps' -f util/status-all.awk < $TEMP
cat $TEMP
