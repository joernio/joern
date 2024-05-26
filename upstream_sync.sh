#!/bin/bash

set -e

git remote add upstream https://github.com/joernio/joern

usage() {
  echo "Usage: $0 [--publish]"
  exit 1
}

PUBLISH=false
while [[ "$#" -gt 0 ]]; do
  case $1 in
    --publish) PUBLISH=true ;;
    *) usage ;;
  esac
  shift
done

git fetch upstream

git checkout sync
git merge upstream/master
git push origin sync

if [ "$PUBLISH" = true ]; then
  git checkout master
  git merge sync
  git push origin master
fi