#!/bin/sh
echo '
  set_dfmode "src";;
  cd "/itt_core";; 
  status_and_abandon_all ();; 
  eprintln "MetaPRL exiting";; ' | \
editor/ml/mp -batch 2>&1 | \
awk -F"[[:blank:]\`']+" '/^Entering / {next}; /^Module: / {MOD=$2}; /^Status:.*incomplete/ { $2 = MOD "/" $2; print $0}'
