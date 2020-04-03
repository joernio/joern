#!/usr/bin/env bash
set -euo pipefail

cd /tmp
rm -rf /tmp/hugo_cache/

if [ -d "/tmp/joern/" ]; then
    echo "removing old joern directory";
    rm -rf joern;
fi

# Clone joern

git clone --depth 1 https://github.com/ShiftLeftSecurity/joern
if [ ! -d "/tmp/joern/" ]; then
    echo "Cloning `joern` failed";
    exit
fi

# clone codepropertygraph

git clone --depth 1 https://github.com/ShiftLeftSecurity/codepropertygraph
if [ ! -d "/tmp/codepropertygraph/" ]; then
    echo "Cloning `codepropertygraph` failed";
    exit
fi

echo "generating site"
cd joern/docs;
hugo;

if [ ! -f "/tmp/joern/docs/public/index.html" ]; then
    echo "Index file not present";
    exit
fi
