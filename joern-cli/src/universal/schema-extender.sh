#!/usr/bin/env sh

set -v

cd schema-extender
source ./cpg-version.sh 
sbt clean replaceDomainClassesInJoern
cd ..
