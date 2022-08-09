#!/usr/bin/env bash

# This script abstracts away API calls to sdkman's HTTP API for publishing new candidate versions.
#
# Example usage:
# ./sdkman_publish.sh 1.2.3

set -o errexit
set -o pipefail

# Script starts here

command -v curl >/dev/null 2>&1 || { echo "'curl' is required but not installed. Exiting." >&2; exit 1; }

if [ "$#" -ne 1 ]; then
    echo "Incorrect number of arguments. Usage: './sdkman_release_new_candidate_version.sh 1.2.3'. Exiting."
    exit 1
fi

if [[ -z "${SDKMAN_CONSUMER_KEY}" ]]; then
  echo "ENV var 'SDKMAN_CONSUMER_KEY' required but not set. Exiting."
  exit 1
fi

if [[ -z "${SDKMAN_CONSUMER_TOKEN}" ]]; then
  echo "ENV var 'SDKMAN_CONSUMER_TOKEN' required but not set. Exiting."
  exit 1
fi

readonly VERSION=$(echo $1 | sed -e 's/^v//') # remove trailing 'v' if found

curl -v -X POST \
-H "Consumer-Key: ${SDKMAN_CONSUMER_KEY}" \
-H "Consumer-Token: ${SDKMAN_CONSUMER_TOKEN}" \
-H "Content-Type: application/json" \
-H "Accept: application/json" \
-d "{\"candidate\": \"joern\", \"version\": \"${VERSION}\", \"url\": \"https://github.com/joernio/joern/releases/download/v${VERSION}/joern-cli.zip\"}" \
https://vendors.sdkman.io/release
