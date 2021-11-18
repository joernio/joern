#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

JOERN="$SCRIPT_ABS_DIR"/../joern

$JOERN --script tests/frontends-testscript.sc --params pathToCode=tests/code/c,methodCountAtLeast=2,expectedMethod=print_number

JOERN_EXIT_CODE=$?
if [ $JOERN_EXIT_CODE != 0 ]; then
  echo "Script [$script] failed to run successfully."
  exit 1
fi

