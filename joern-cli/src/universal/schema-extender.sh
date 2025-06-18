#!/usr/bin/env sh

cd schema-extender
export CPG_VERSION=$(cat cpg-version)
./sbt clean replaceDomainClassesInJoern
cd ..
