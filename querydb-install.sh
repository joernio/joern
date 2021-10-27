#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset
set -eu

if [ "$(uname)" = 'Darwin' ]; then
  # get script location
  # https://unix.stackexchange.com/a/96238
  if [ "${BASH_SOURCE:-x}" != 'x' ]; then
    this_script=$BASH_SOURCE
  elif [ "${ZSH_VERSION:-x}" != 'x' ]; then
    setopt function_argzero
    this_script=$0
  elif eval '[[ -n ${.sh.file} ]]' 2>/dev/null; then
    eval 'this_script=${.sh.file}'
  else
    echo 1>&2 "Unsupported shell. Please use bash, ksh93 or zsh."
    exit 2
  fi
  relative_directory=$(dirname "$this_script")
  SCRIPT_ABS_DIR=$(cd "$relative_directory" && pwd)
else
  SCRIPT_ABS_PATH=$(readlink -f "$0")
  SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
fi

# Check required tools are installed.
check_installed() {
  if ! type "$1" > /dev/null; then
    echo "Please ensure you have $1 installed."
    exit 1
  fi
}


# https://stackoverflow.com/a/28085062
: ${DEFAULT_JOERN_INSTALL_DIR:=$PWD/joern-cli/target/universal/stage}
echo "where is the joern distribution installed?
note: you can e.g. create one locally using 'sbt stage'
[$DEFAULT_JOERN_INSTALL_DIR]:"
read JOERN_INSTALL_DIR
if [ -z "$JOERN_INSTALL_DIR" ]; then
  JOERN_INSTALL_DIR=$DEFAULT_JOERN_INSTALL_DIR
fi

echo "building the plugin"
sbt querydb/createDistribution
readonly QUERYDB_ZIP=$PWD/querydb/target/querydb.zip

echo "Installing plugin"
pushd $JOERN_INSTALL_DIR
  ./joern --remove-plugin querydb
  ./joern --add-plugin $QUERYDB_ZIP
popd
