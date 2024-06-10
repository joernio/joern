#!/usr/bin/env sh

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname $SCRIPT_ABS_PATH)
LOG4J2_CONFIG="${SCRIPT_ABS_DIR}/src/main/resources/log4j2.xml"

"${SCRIPT_ABS_DIR}/target/universal/stage/bin/c2cpg.sh" -Dlog4j2.formatMsgNoLookups=true -Dlog4j.configurationFile="${LOG4J2_CONFIG}" "$@"
