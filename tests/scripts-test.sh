#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

# Setup
JOERN="$SCRIPT_ABS_DIR"/../joern
JOERN_PARSER="$SCRIPT_ABS_DIR"/../joern-parse
JOERN_SCRIPTS_DIR="$SCRIPT_ABS_DIR/../scripts"

# Generate a CPG for use in the script tests.
$JOERN_PARSER "$SCRIPT_ABS_DIR"/code

# Run each script.
for script in "$JOERN_SCRIPTS_DIR"/**/*.sc; do
  echo "Testing script [$script]..."
  $JOERN --import "$SCRIPT_ABS_DIR"/loadcpg.sc --script "$script"
  JOERN_EXIT_CODE=$?
  if [ $JOERN_EXIT_CODE != 0 ]; then
    echo "Script [$script] failed to run successfully."
    exit 1
  fi

  echo "Script [$script] passed..."
  echo ""
done

echo "All scripts tested successfully."
