#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

# Setup
JOERN_PARSER="$SCRIPT_ABS_DIR"/../joern-parse
JOERN_PARSER_TEST_DIR="$SCRIPT_ABS_DIR"/smoke-test-repos

JOERN_PARSER_TEST_PROJECTS=(
  "https://github.com/sobotka/blender.git;blender"
  "https://github.com/electron/electron.git;electron"
  "https://github.com/FFmpeg/FFmpeg.git;FFmpeg"
  "https://github.com/git/git.git;git"
  # TODO: OpenCV fails due to StackOverflowError in Antlr.
  # "https://github.com/opencv/opencv.git;opencv"
  "https://github.com/antirez/redis.git;redis"
  "https://github.com/tensorflow/tensorflow.git;tensorflow"
  "https://github.com/microsoft/terminal.git;terminal"
)

if ! type "git" > /dev/null; then
  echo "Please ensure Git is installed."
  exit 1
fi

if [ ! -e "$JOERN_PARSER" ]; then
  echo "The parser executable could not be found. Have you run 'sbt stage'?"
  exit 1
fi

mkdir -p "$JOERN_PARSER_TEST_DIR"
cd "$JOERN_PARSER_TEST_DIR" || exit 0

# Run parser over each test project.
for JOERN_PARSER_PROJECT_TUPLE in "${JOERN_PARSER_TEST_PROJECTS[@]}"
do
  # Extract git ref and project name.
  JOERN_PARSER_TEST_GIT_REF=$(echo "$JOERN_PARSER_PROJECT_TUPLE" | cut -d ";" -f 1)
  JOERN_PARSER_TEST_PROJECT=$(echo "$JOERN_PARSER_PROJECT_TUPLE" | cut -d ";" -f 2)

  # Clone project & run parser.
  echo "Testing project [$JOERN_PARSER_TEST_PROJECT]"
  [ ! -d "$JOERN_PARSER_TEST_PROJECT" ] && git clone --quiet "$JOERN_PARSER_TEST_GIT_REF"
  "$JOERN_PARSER" \
    -J-Xmx12G \
    --source-file-ext=".cc,.hh" \
    --out="$JOERN_PARSER_TEST_DIR/$JOERN_PARSER_TEST_PROJECT.bin" \
    "$JOERN_PARSER_TEST_DIR/$JOERN_PARSER_TEST_PROJECT"

  # Check status of parse
  TEST_EXIT_CODE=$?
  if [ $TEST_EXIT_CODE = 0 ]; then
    echo "Test for project [$JOERN_PARSER_TEST_PROJECT] passed."
  else
    echo "Test for project [$JOERN_PARSER_TEST_PROJECT] failed with exit code [$TEST_EXIT_CODE]."
    break
  fi
done

# Cleanup
cd - || exit 0
rm -rf "$JOERN_PARSER_TEST_DIR"
