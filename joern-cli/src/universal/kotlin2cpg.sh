#!/usr/bin/env sh

# Script starts here

readonly SCRIPT_DIR=$(dirname "$(realpath "$0")")
readonly BIN="${SCRIPT_DIR}/frontends/kotlin2cpg/bin/kotlin2cpg"

"${BIN}" -Dlog4j2.formatMsgNoLookups=true \
  "$@"
