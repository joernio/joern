#!/usr/bin/env sh

OLDPWD=$(pwd)

cd schema-extender
sbt clean replaceDomainClassesInJoern

cd $OLDPWD
