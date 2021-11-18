#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

JOERN="$SCRIPT_ABS_DIR"/../joern

languages=(c cat mouse)
declare -A minMethodCount=(
  [c]=20
  [javasrc]=10000
  [java]=10000
  [js]=10000
)
declare -A expectedMethod=(
  [c]=print_number
  [javasrc]=foo
  [java]=foo
  [js]=foo
)

for language in "${languages[@]}"; do
  $JOERN --script tests/frontends-testscript.sc --params pathToCode=tests/code/$language,minMethodCount=${minMethodCount[$language]},expectedMethod=${expectedMethod[$language]}

  JOERN_EXIT_CODE=$?
  if [ $JOERN_EXIT_CODE != 0 ]; then
    echo "Script [$script] failed to run successfully for language=$language"
    exit 1
  fi
done


