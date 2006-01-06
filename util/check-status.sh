#!/bin/sh

# WARNING: Do not run this script directly!
# Run the util/check-status script instead.

# Standardize the terminal environment
export TERM=xterm LANG=C

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
cd $1
TMPDIR=`mktemp -d /tmp/mp-status-check.XXXXXX`
LOGS=$TMPDIR/$ST_NAME
REMOTE_LOGS=$REMOTE_DIR$ST_NAME
if [ -z "$LOGNAME" ]; then
   LOGNAME=`whoami`
fi
if [ ! -e editor/ml/svnversion.txt ]; then
   echo editor/ml/svnversion.txt does not exist!
   exit 1
fi
MYREV="`cat editor/ml/svnversion.txt| sed -e 's/.*://' -e 's/M//'`"
REV=$MYREV
until [ -n "$LOG" -a -f "$LOG" -a -s "$LOG" ]; do
   if [ "$REV" -lt 8410 ]; then
      echo "Tried revisions $MYREV down to 8410, can not find status logs at $REMOTE_DIR!" | \
      mail -s "MetaPRL proofs status can not be checked!" "$LOGNAME"
      exit 1
   fi
   SUFFIX=-rev"$REV".txt
   LOG=$LOGS$SUFFIX
   (cd $TMPDIR; $GET $REMOTE_LOGS$SUFFIX ) &>/dev/null
   REV=`expr "$REV" - 1`
done
REV=`expr "$REV" + 1`
TEMP=`mktemp /tmp/mkstatus.XXXXXX`
umask 002
shift
RUN_OMAKE=yes
MP_DEBUG=
PROG=util/status-all.sh
SUBJECT="proofs status update"
while [ $# -gt 0 ]; do
   if [ "$1" = "update" -o "$1" = "status" ]; then
      echo "*** svn status ***"
      svn status
      echo ""
   elif [ "$1" = "nomake" ]; then
      RUN_OMAKE=
   elif [ "$1" = "MP_DEBUG" ]; then
      shift
      echo "MP_DEBUG environment variable is set to \`$1'"
      export MP_DEBUG="$1"
   elif [ "$1" = "core" ]; then
      PROG=util/core-incompletes.sh
      SUBJECT="- incomplete proofs in core"
   else
      echo WARNING:
      echo "WARNING: Unknown command line option: $1"
      echo WARNING:
   fi
   shift
done > $TEMP 2>&1
((
   cat $TEMP
   if [ "$RUN_OMAKE" ]; then
      rm -f editor/ml/mp.opt
      unset OMAKEFLAGS
      omake VERBOSE=1 -S editor/ml/mp.opt
      sleep 10
   fi
   if [ -f editor/ml/mp.opt ]; then
      $PROG > $TEMP
      if [ "$PROG" = "util/status-all.sh" ]; then
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
         cat $TEMP
      fi
   else
      echo ""
      echo BUILD FAILED!
   fi
) 2>&1 ) | mail -s "MetaPRL $SUBJECT (`hostname -s`, `pwd`, rev $REV->`cat editor/ml/svnversion.txt`)" "$LOGNAME"
rm -rf $TMPDIR
