#!/bin/sh
cd `dirname $0`
for i in $*; do
   ./test_it.sh $i
done
