#!/bin/sh
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
cd "`dirname $0`"
SAMPLE=`./xfontsel-pattern.sh`
xfontsel -sampleUCS "$SAMPLE" -pattern '*-iso10646-*'
