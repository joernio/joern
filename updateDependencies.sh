#!/usr/bin/env bash
NON_INTERACTIVE_OPTION=$1

function update {
  local NAME=$1
  local REPO=$2

  local HIGHEST_TAG=`git ls-remote --tags $REPO | awk -F"/" '{print $3}' | grep '^v[0-9]*\.[0-9]*\.[0-9]*' | grep -v {} | sort --version-sort | tail -n1`
  # drop initial v from git tag
  local VERSION=${HIGHEST_TAG:1}
  local SEARCH="val ${NAME}Version\([ ]*\)= .*"
  local REPLACE="val ${NAME}Version\1= \"$VERSION\""

  if [ "$NON_INTERACTIVE_OPTION" == "--non-interactive" ]; then
    echo "non-interactive mode, auto-updating all dependencies"
    sed -i "s/$SEARCH/$REPLACE/" build.sbt
  else
    echo "set version for $NAME to $VERSION? [Y/n]"
    read ANSWER
    if [ -z $ANSWER ] || [ "y" == $ANSWER ] || [ "Y" == $ANSWER ]; then
      sed -i "s/$SEARCH/$REPLACE/" build.sbt
    fi
  fi
}

update cpg git@github.com:ShiftLeftSecurity/codepropertygraph.git
update fuzzyc git@github.com:ShiftLeftSecurity/fuzzyc2cpg.git
