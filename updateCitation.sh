#!/usr/bin/env bash
VERSION=$1
DATE=$2

cat CITATION.template > CITATION.cff
echo "version: ${VERSION:1}" >> CITATION.cff
echo "date-released: $DATE" >> CITATION.cff
