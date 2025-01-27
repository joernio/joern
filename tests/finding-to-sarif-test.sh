#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
JOERN_TESTS_DIR=$(dirname "$SCRIPT_ABS_PATH")
JOERN="$JOERN_TESTS_DIR"/..

mkdir -p /tmp/sarif
./joern-scan "$JOERN_TESTS_DIR/code/sarif-test" --store
./joern --script "$JOERN_TESTS_DIR/test-sarif.sc" --param cpgFile="$JOERN/workspace/sarif-test/cpg.bin" --param outFile="/tmp/sarif/test.sarif"
exit_code=$(curl -s -X POST \
    -F "postedFiles=@/tmp/sarif/test.sarif;type=application/octet-stream" \
    https://sarifweb.azurewebsites.net/Validation/ValidateFiles | jq -r '.exitCode')

echo "SARIF Validation Exit Code: $exit_code"

exit $exit_code
