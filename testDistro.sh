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
  # for i in {0..9}; do
    # echo "XX0 run $i start"
    # tests/frontends-tests.sh 
    # exit_code=$?
    # echo "XX0 run $i end; exit_code=$exit_code"
  # done

  tests/frontends-tests.sh 

  # exit_code=$?
  # TODO: only if failed, copy libs to 'artifacts' and upload them
  mkdir target/debug-artifacts
  echo test > target/debug-artifacts/test.txt
  cp -rp joern-cli/target/universal/stage/lib/* target/debug-artifacts
  # if ...
popd

echo "success. go analyse some code"
