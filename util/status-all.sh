#!/bin/sh
TEMP=`mktemp /tmp/mkstatus2.XXXXXX`
TIME=`mktemp /tmp/mktime.XXXXXX`
echo 'cd "/";; set_dfmode "src";; status_and_abandon_all ();; eprintln "MetaPRL exiting";; ' | \
/usr/bin/time -f 'Expand time: %Uuser %Ssystem %Eelapsed (%PCPU); pagefaults: %Fmajor+%Rminor; %Wswaps' -o $TIME editor/ml/mpopt -batch >$TEMP 2>&1
cat $TIME
rm -f $TIME
gawk -F ' proof [(]| rule boxes,| primitive steps' -f util/status-all.awk < $TEMP
gawk -F"[[:blank:]\`']+" '/^Entering / {next}; /^Module: / {MOD=$2}; /^Status:/ { $2 = MOD "/" $2 }; {print $0}' < $TEMP
