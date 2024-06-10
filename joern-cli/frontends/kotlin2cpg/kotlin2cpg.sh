#!/usr/bin/env sh

readonly SCRIPT_DIR=$(dirname "$(realpath "$0")")
readonly BIN="${SCRIPT_DIR}/target/universal/stage/bin/kotlin2cpg"
readonly LOG4J2_CONFIG="${SCRIPT_DIR}/src/main/resources/log4j2.xml"

"${BIN}" -Dlog4j2.formatMsgNoLookups=true -Dlog4j.configurationFile="${LOG4J2_CONFIG}" \
  "$@"
