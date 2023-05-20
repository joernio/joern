#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

# Setup
JOERN="$SCRIPT_ABS_DIR"/../joern
JOERN_SCRIPTS_DIR="$SCRIPT_ABS_DIR/../scripts"
TESTCODE_ROOT="$SCRIPT_ABS_DIR/../joern-cli/src/test/resources/testcode"

# Run all scripts
scripts=(
    c/pointer-to-int.sc
    c/syscalls.sc
    c/userspace-memory-access.sc
    c/malloc-overflow.sc
    c/malloc-leak.sc
    c/const-ish.sc
)
declare -A code=(
  [c/pointer-to-int.sc]=unsafe-ptr
  [c/syscalls.sc]=syscalls
  [c/userspace-memory-access.sc]=syscalls
  [c/malloc-overflow.sc]=malloc-overflow
  [c/malloc-leak.sc]=leak
  [c/const-ish.sc]=const-ish
)

for script in "${scripts[@]}"; do
  $JOERN --script "$JOERN_SCRIPTS_DIR/$script" --param inputPath="$TESTCODE_ROOT/${code[$script]}"
  JOERN_EXIT_CODE=$?

  if [ $JOERN_EXIT_CODE != 0 ]; then
    echo "Script [$script] failed to run successfully."
    exit 1
  fi
  echo "Script [$script] passed..."
  echo ""
done

echo "All scripts tested successfully."
