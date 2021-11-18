#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

JOERN="$SCRIPT_ABS_DIR"/../joern

frontends=(c javascript javasrc ghidra)
# frontends=(c javascript javasrc jvm ghidra)
# TODO add 'jvm' - will talk to david re jimple
declare -A minMethodCount=(
  [c]=2
  [javascript]=3
  [javasrc]=7
  [jvm]=7
  [ghidra]=100
)
declare -A expectedMethod=(
  [c]=print_number
  [javascript]=lookForProperty
  [javasrc]=callsExternalMethod
  [jvm]=callsExternalMethod
  [ghidra]=reallocarray
)

for frontend in "${frontends[@]}"; do
  $JOERN --script tests/frontends-testscript.sc --params inputPath=tests/code/$frontend,minMethodCount=${minMethodCount[$frontend]},expectedMethod=${expectedMethod[$frontend]},frontend=$frontend

  JOERN_EXIT_CODE=$?
  if [ $JOERN_EXIT_CODE != 0 ]; then
    echo "Script [$script] failed to run successfully for frontend=$frontend"
    exit 1
  fi

  echo "Frontend [$frontend] tested successfully"
done


