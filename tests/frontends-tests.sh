#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

JOERN="$SCRIPT_ABS_DIR"/../joern

# first, run a simplistic smoke test
$JOERN --script tests/frontends-smoketest.sc

# now test all frontends with proper input code
frontends=(c jssrc javasrc java ghidra pythonsrc php)
declare -A minMethodCount=(
  [c]=2
  [jssrc]=3
  [javasrc]=7
  [java]=7
  [ghidra]=100
  [pythonsrc]=2
  [php]=3
)
declare -A expectedMethod=(
  [c]=print_number
  [jssrc]=lookForProperty
  [javasrc]=callsExternalMethod
  [java]=callsExternalMethod
  [ghidra]=reallocarray
  [pythonsrc]=my_fun
  [php]=foo
)

for frontend in "${frontends[@]}"; do
  rm -rf workspace
  $JOERN --script tests/frontends-testscript.sc --param inputPath=tests/code/$frontend --param minMethodCount=${minMethodCount[$frontend]} --param expectedMethod=${expectedMethod[$frontend]} --param frontend=$frontend

  JOERN_EXIT_CODE=$?
  if [ $JOERN_EXIT_CODE != 0 ]; then
    echo "Script [$script] failed to run successfully for frontend=$frontend"
    exit 1
  fi

  echo "Frontend [$frontend] tested successfully"
done


