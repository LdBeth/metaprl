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
      echo "Tried last 100 days, can not find status logs at $REMOTE_DIR!" | \
      mail -s "MetaPRL proofs status can not be checked!" "$LOGNAME"
      exit 1
   fi
   SUFFIX=`date -d "$DAYS days ago" +-%Y.%m.%d.txt`
   LOG=$LOGS$SUFFIX
   (cd $TMPDIR; $GET $REMOTE_LOGS$SUFFIX ) &>/dev/null
   DAYS=`expr "$DAYS" + 1`
done
TEMP=`mktemp /tmp/mkstatus.XXXXXX`
umask 002
cd $1
((
if [ "$2" = "update" ]; then
   echo "*** cvs -n update ***"
   ( cvs -n update 2>&1 ) | grep -v '^cvs server: New directory'
   echo ""
fi
# cvs -q update 2>&1
if [ "$3" != "nomake" ]; then
   rm -f editor/ml/mp.opt
   unset OMAKEFLAGS
   omake VERBOSE=1 -S editor/ml/mp.opt
   sleep 10
fi
if [ -f editor/ml/mp.opt ]; then
util/status-all.sh > $TEMP
if diff -q -I '^Expand time:' $TEMP $LOG > /dev/null; then
   echo ""
   echo NO PROOF STATUS CHANGES
   echo ""
   echo -n Was:
   grep '^Expand time:' $LOG
   echo -n Now:
   grep '^Expand time:' $TEMP
   echo ""
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
gawk -F"[[:blank:]\`']+" '/incomplete/ { print $2}; {next}' < $TEMP
else
   echo ""
   echo BUILD FAILED!
fi
 ) 2>&1 ) | mail -s "MetaPRL proofs status update (`hostname -s`, `pwd`)" "$LOGNAME"
rm -rf $TMPDIR
