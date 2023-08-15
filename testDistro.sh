#!/bin/bash

set -e # Stop on error
set -u # Treat unset variables as an error
# set -x # Uncomment this for debugging

# Define log file path
LOG_FILE="target/test_distro.log"
mkdir -p target

# Function to log messages with timestamp
log() {
    echo "$(date "+%Y-%m-%d %H:%M:%S") - $1" | tee -a "$LOG_FILE"
}

# Absolute paths
readonly SCRIPT_ABS_PATH=$(readlink -f "$0")
readonly SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
readonly REPO_ROOT=$SCRIPT_ABS_DIR

log "Staging joern"
pushd $REPO_ROOT || { log "Failed to change directory to $REPO_ROOT"; exit 1; }

sbt -Dsbt.log.noformat=true clean joerncli/stage querydb/createDistribution || { log "sbt tasks failed"; exit 1; }

# Run the tests
tests=( "frontends-tests.sh" "scripts-test.sh" "querydb-test.sh" )
for test in "${tests[@]}"; do
    log "Running $test"
    tests/"$test" || { log "Test $test failed"; exit 1; }
done

popd || { log "Failed to change back to the original directory"; exit 1; }

log "Success. Go analyse some code."
