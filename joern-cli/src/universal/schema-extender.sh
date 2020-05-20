#!/usr/bin/env sh

if [ "$(uname -s)" = "Darwin" ]; then
    SCRIPT_ABS_PATH=$(greadlink -f "$0")
else
    SCRIPT_ABS_PATH=$(readlink -f "$0")
fi

SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")/schema-extender
SCRIPT="$SCRIPT_ABS_DIR"/bin/schema-extender

if [ ! -f "$SCRIPT" ]; then
    echo "Unable to find $SCRIPT, have you created the distribution?"
    exit 1
fi

OLDPWD=$(pwd)
cd $SCRIPT_ABS_DIR
TARGET_JAR=$(ls ../lib/io.shiftleft.codepropertygraph_2.13-*)

$SCRIPT -J-XX:+UseG1GC -J-XX:CompressedClassSpaceSize=128m --target=$TARGET_JAR --scalac=../bin/scalac "$@"

cd $OLDPWD
