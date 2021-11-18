#!/bin/bash
set -e #stop on error
# set -x #verbose on

readonly SCRIPT_ABS_PATH=$(readlink -f "$0")
readonly SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
readonly REPO_ROOT=$SCRIPT_ABS_DIR

echo "staging joern"
pushd $REPO_ROOT
  sbt -Dsbt.log.noformat=true clean joerncli/stage querydb/createDistribution
  tests/querydb-test.sh
  tests/frontends-tests.sh
  tests/scripts-test.sh
popd

echo "success. go analyse some code"
