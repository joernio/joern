#!/bin/bash
# set -e #stop on error
# set -x #verbose on

readonly SCRIPT_ABS_PATH=$(readlink -f "$0")
readonly SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
readonly REPO_ROOT=$SCRIPT_ABS_DIR

echo "staging joern"
pushd $REPO_ROOT
  sbt -Dsbt.log.noformat=true clean joerncli/stage

  # run test 10 times - sometimes fails...
  for i in {0..9}; do
    echo "XX0 run $i start"
    tests/frontends-tests.sh 
    echo "XX0 run $i end"
  done
popd

echo "success. go analyse some code"
