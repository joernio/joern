#!/bin/bash
# set -e #stop on error
# set -x #verbose on

readonly SCRIPT_ABS_PATH=$(readlink -f "$0")
readonly SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
readonly REPO_ROOT=$SCRIPT_ABS_DIR

echo "staging joern"

pushd $REPO_ROOT
  # sbt -Dsbt.log.noformat=true clean joerncli/stage


  for i in {0..9}; do
    echo "XX0 run $i start"
    sbt -Dsbt.log.noformat=true clean joerncli/stage
    tests/frontends-tests.sh 
    exit_code=$?
    echo "XX0 run $i end; exit_code=$exit_code"

    mkdir -p target/debug-artifacts/$i
    if [ $exit_code -ne 0 ]
    then
      echo "XX1 error"
      cp -rp joern-cli/target/universal/stage/lib/* target/debug-artifacts/$i
    else
      echo "XX1 success"
      echo success > target/debug-artifacts/$i/success.txt
    fi
  done

popd
