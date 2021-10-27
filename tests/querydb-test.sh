#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

readonly REPO_ROOT=$SCRIPT_ABS_DIR/../
readonly JOERN_STAGE_DIR=$REPO_ROOT/joern-cli/target/universal/stage
readonly QUERYDB_ZIP=$REPO_ROOT/querydb/target/querydb.zip

echo "Installing querydb"
cd $JOERN_STAGE_DIR
./joern --remove-plugin querydb
./joern --add-plugin $QUERYDB_ZIP

readonly QUERY_SQLI_COUNT=$(./joern-scan --list-query-names | grep "sql-injection" | wc -l)
if [[ $QUERY_SQLI_COUNT -eq 0 ]]; then
  echo "error: query 'sql-injection' from querydb not found - something wrong with the querydb installation?"
  exit 1
fi

echo "querydb integration tested successfully."
