#!/bin/sh

# WARNING: Do not run this script directly!
# Run the util/check-status script instead.

REMOTE_DIR=http://files.metaprl.org/logs/
ST_NAME=status_all
if [ -x /usr/bin/wget ]; then
   GET=/usr/bin/wget
elif [ -x /usr/bin/lftpget ]; then
   GET=/usr/bin/lftpget
else
   GET=wget
fi

if [ ! -d "$1" ]; then
   echo '$1' should be a directory!
   exit 1
fi
TMPDIR=`mktemp -d /tmp/mp-status-check.XXXXXX`
LOGS=$TMPDIR/$ST_NAME
REMOTE_LOGS=$REMOTE_DIR$ST_NAME
if [ -z "$LOGNAME" ]; then
   LOGNAME=`whoami`
fi
DAYS=0
until [ -n "$LOG" -a -f "$LOG" -a -s "$LOG" ]; do
   if [ "$DAYS" -gt 50 ]; then
      echo "Tried last 50 days, can not find status logs at $REMOTE_DIR!" | \
      mail -s "MetaPRL proofs status can not be checked!" "$LOGNAME"
      exit 1
   fi
   SUFFIX=`date -d "$DAYS days ago" +-%Y.%m.%d.txt`
   LOG=$LOGS$SUFFIX
   (cd $TMPDIR; $GET $REMOTE_LOGS$SUFFIX 2>&1)>/dev/null
   DAYS=`expr "$DAYS" + 1`
done
TEMP=`mktemp /tmp/mkstatus.XXXXXX`
TEMP2=`mktemp /tmp/mkstatus2.XXXXXX`
TIME=`mktemp /tmp/mktime.XXXXXX`
umask 002
cd $1
rm -f editor/ml/mp.opt
( cvs -q update 2>&1
(make -s depend > /dev/null && make -s opt > /dev/null) 2>&1 | egrep -v -- "-jN forced in submake|Makefile.dep: No such file or directory"
if [ -f editor/ml/mp.opt ]; then
sleep 10
echo 'set_dfmode "src";; status_all ();;' | /usr/bin/time -o $TIME editor/ml/mpopt > /dev/null 2>$TEMP2
echo ""
echo "Expand time:"
cat $TIME
rm -f $TIME
echo "Complete proofs:" > $TEMP
grep "derived object with a complete proof" $TEMP2 | wc -l >> $TEMP
echo "" >> $TEMP
echo "Incomplete proofs:" >> $TEMP
grep "derived object with an incomplete proof" $TEMP2 | wc -l >> $TEMP
echo "" >> $TEMP
cat $TEMP2 >> $TEMP
if diff -q -I '^User time' $TEMP $LOG > /dev/null; then
   echo ""
   echo NO PROOF STATUS CHANGES
else
   echo ""
   echo PROOF STATUS CHANGES:
   echo ""
   diff -u $LOG $TEMP
fi
echo ""
echo "A complete log can be found in $TEMP on `hostname -s`"
echo ""
echo "Complete list off all rules with incomplete proofs:"
gawk -F"[[:blank:]\`']+" '/Module: / {MOD=$2}; /incomplete/ { print MOD "/" $2}; {next}' < $TEMP
else
echo ""
echo BUILD FAILED!
fi
 ) | mail -s "MetaPRL proofs status update" "$LOGNAME"
rm -rf $TMPDIR
