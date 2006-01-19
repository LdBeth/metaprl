#!/bin/sh
cd `dirname $0`/..
echo " ---- "
echo $1
(./mpopt tests/$1; ./mpopt tests/$1; ./mpopt tests/$1; ./mpopt tests/$1; ./mpopt tests/$1) 2>&1 | grep "Real time"
echo "      "
