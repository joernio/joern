#!/usr/bin/env sh

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname $SCRIPT_ABS_PATH)

exec $SCRIPT_ABS_DIR/target/universal/stage/bin/ghidra2cpg.sh $@
