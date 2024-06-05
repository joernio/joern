#!/bin/bash

set -e

git remote add upstream https://github.com/joernio/joern

usage() {
  echo "Usage: $0 [--publish] [--branch <branch>]"
  exit 1
}

PUBLISH=false
BRANCH=""
while [[ "$#" -gt 0 ]]; do
  case $1 in
    --publish) PUBLISH=true ;;
    --branch) BRANCH="$2"; shift ;;
    *) usage ;;
  esac
  shift
done

if [ -z "$BRANCH" ]; then
  usage
fi

git fetch upstream

git checkout -b "$BRANCH"
git merge upstream/master
git push origin "$BRANCH"

if [ "$PUBLISH" = true ]; then
  git checkout master
  git merge "$BRANCH"
  git push origin master
fi