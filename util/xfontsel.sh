#!/bin/sh
cd "`dirname $0`"
SAMPLE=`./xfontsel-pattern.sh`
xfontsel -sampleUCS "$SAMPLE" -pattern '*-iso10646-*'
