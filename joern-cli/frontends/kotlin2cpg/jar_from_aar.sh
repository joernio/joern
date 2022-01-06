#!/usr/bin/env bash

#
# This script unpacks an aar file and copies its containing `classes.jar`
# into the current directory, with the name composed of the input aar file minus the extension plus `jar`.
#
# Usage:
#    $ ./jar_from_aar.sh lifecycle-viewmodel-ktx-2.2.0.aar
#       /tmp/tmp.p1wRlFC11u ~/src/myjars
#       ~/src/myjars
#       Jar extracted from 'lifecycle-viewmodel-ktx-2.2.0.aar' to 'lifecycle-viewmodel-ktx-2.2.0.jar'.
#       Done.
#

set -o errexit
set -o pipefail

# Script starts here

readonly AAR_FILE=$1
if ! [ -f "$AAR_FILE" ]; then
    echo "Input file does not exist at '${AAR_FILE}'. Exiting."
    exit 1
fi

if ! [[ $AAR_FILE == *aar ]]; then
  echo "Input file does not end in 'aar'. Exiting."
  exit 1
fi

readonly TEMP_DIR=$(mktemp -d)
if ! [[ $? == 0 ]]; then
  echo "Failed to create temporary directory. Exiting."
  exit 1
fi

function cleanup_tmp_dir {
  rm -rf "${TEMP_DIR}"
}
trap cleanup_tmp_dir EXIT

readonly AAR_BASENAME=$(basename "${AAR_FILE}")
readonly AAR_IN_TEMP="${TEMP_DIR}/${AAR_BASENAME}"
readonly AAR_FILENAME=${AAR_IN_TEMP##*/}
readonly OUT_JAR="${AAR_FILENAME%.*}.jar"

cp "${AAR_FILE}" "${AAR_IN_TEMP}"
pushd "${TEMP_DIR}"
unzip -qq "${AAR_IN_TEMP}"
popd

cp "${TEMP_DIR}/classes.jar" "${OUT_JAR}"
echo "Jar extracted from '${AAR_FILE}' to '${OUT_JAR}'."

echo "Done."

