#!/usr/bin/env bash

# Script starts here

readonly SCRIPT_DIR=$(dirname "$(realpath "$0")")
readonly BIN="${SCRIPT_DIR}/target/universal/stage/bin/pysrc2cpg"
readonly LOG4J2_CONFIG="${SCRIPT_DIR}/src/main/resources/log4j2.xml"

exec "${BIN}" -Dlog4j2.formatMsgNoLookups=true -Dlog4j.configurationFile="${LOG4J2_CONFIG}" \
  "$@"
