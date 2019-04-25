#!/bin/sh

FUZZYC2CPG_DIR="fuzzyc2cpg"

if [ ! -d $FUZZYC2CPG_DIR ]; then
  git clone https://github.com/ShiftLeftSecurity/fuzzyc2cpg.git
else
  DIR=$(pwd)
  cd $FUZZYC2CPG_DIR
  git pull
  cd $DIR
fi

DIR=$(pwd)
cd $FUZZYC2CPG_DIR
sbt stage
cd $DIR

sbt stage
