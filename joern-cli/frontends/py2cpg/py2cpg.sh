#!/bin/sh
dir=`dirname $0`

java -Dlog4j.configurationFile=$dir/log4j2.xml -jar $dir/out/py2cpg/assembly/dest/out.jar $@
