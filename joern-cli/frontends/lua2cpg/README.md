# lua2cpg Preparation

This directory exists only on the preparation branch.

It is a scaffold for future upstream work and does not claim:

- final maintainer-approved frontend identity
- final schema or traversal API shape
- QueryDB readiness
- upstream-ready Lua support

## E1 Local Smoke

```bash
sbt 'lua2cpg/testOnly io.joern.lua2cpg.DLinkLuCIEntrySmokeTest'
sbt 'lua2cpg/test'
sbt 'lua2cpg/stage'
```

Expected result:

- `DLinkLuCIEntrySmokeTest` observes `META_DATA.language == "LUA"`.
- `DLinkLuCIEntrySmokeTest` observes `cgi.lua`, `uci.lua`, and `version.lua`
  as CPG `FILE` nodes.
- `lua2cpg/stage` exits `0` and produces a staged `lua2cpg` command.

This E1 smoke proves only the runnable Lua frontend entry and file inventory.
It does not claim Lua parsing, bytecode decode, AST semantics, dataflow,
QueryDB, sanitizer, or report construction.

## Bytecode Model Smoke

```bash
sbt 'lua2cpg/testOnly io.joern.lua2cpg.BytecodeModelSmokeTest'
sbt 'lua2cpg/stage'
git status --short
```

Expected result:

- `BytecodeModelSmokeTest` succeeds.
- `lua2cpg/stage` succeeds.
- `git status --short` is clean after the smoke.

This E2 smoke proves bytecode artifact/profile/prototype/constant/diagnostic
CPG evidence only. It does not claim parser AST semantics, dataflow, QueryDB,
sanitizer, report construction, schema extension, distribution acceptance, or
official frontend acceptance.

## Intraprocedural Semantics Smoke

```bash
sbt 'lua2cpg/testOnly io.joern.lua2cpg.IntraproceduralSemanticsSmokeTest'
sbt 'lua2cpg/stage'
git status --short
```

Expected result:

- `IntraproceduralSemanticsSmokeTest` succeeds.
- `lua2cpg/stage` succeeds.
- `git status --short` is clean after the smoke.

This E3 smoke proves bytecode-local intraprocedural CPG evidence through
`CALL`, `IDENTIFIER`, `LITERAL`, `METHOD`, and `REACHING_DEF` evidence over
committed focused `.luac` fixtures. It does not claim interprocedural
arg/return, module require/export resolution, source parser AST semantics,
QueryDB, sanitizer classification, report construction, schema extension
acceptance, distribution acceptance, or official frontend acceptance.

## Interprocedural Module Taint Smoke

```bash
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' \
  sbt 'lua2cpg/testOnly io.joern.lua2cpg.InterproceduralModuleTaintSmokeTest'
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' \
  sbt 'lua2cpg/stage'
git status --short
```

Expected result:

- `InterproceduralModuleTaintSmokeTest` succeeds.
- `lua2cpg/stage` succeeds.
- `git status --short` is clean after the smoke.

This E4 smoke proves interprocedural arg/return, literal require resolution,
module returned-table exports, cross-boundary call targets, and explainable
taint paths over committed fixtures.

It does not claim QueryDB readiness, source parser AST semantics, sanitizer
classification, report construction, schema extension acceptance, distribution
acceptance, or official frontend acceptance.

## Rules, Sanitizer, And Report Smoke

```bash
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' \
  sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'
git status --short
```

Expected result:

- `RulesSanitizerReportSmokeTest` succeeds.
- `git status --short` is clean after the smoke.

This E5 smoke proves source/sink rule matches, sanitizer calls and
classification, report classification, vulnerability report construction, and
explainable E5 boundaries through schema-safe CPG `CALL` markers over committed
focused `.luac` fixtures.

Reviewer traversal surface:

```scala
cpg.call.nameExact("lua.rule.match").code.l
cpg.call.nameExact("lua.source.endpoint").code.l
cpg.call.nameExact("lua.sink.endpoint").code.l
cpg.call.nameExact("lua.sanitizer.call").code.l
cpg.call.nameExact("lua.sanitizer.classification").code.l
cpg.call.nameExact("lua.report.classification").code.l
cpg.call.nameExact("lua.report.vulnerability").code.l
cpg.call.nameExact("lua.e5.boundary").code.l
```

This phase does not claim QueryDB inclusion, schema extension acceptance,
distribution acceptance, source parser AST support, production security-query
readiness, or official frontend acceptance.

## Controller Benchmark Runbook

Quick local capability smoke from the Joern clone:

```bash
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' \
  sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' \
  sbt 'lua2cpg/stage'
joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg --help
git status --short
```

Expected result:

- `RulesSanitizerReportSmokeTest` succeeds and prints E5 node/report counts.
- `lua2cpg/stage` succeeds.
- The staged `lua2cpg --help` command prints usage.
- `git status --short` is clean after the smoke.

referenceAnalyzer-managed benchmark adapter smoke:

```bash
referenceAnalyzer=/path/to/referenceAnalyzer
JOERN_CLONE=$(pwd)
JOERN_COMMAND="$JOERN_CLONE/joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg"
RUN_ID=joern-upstream-smoke
RUN_ROOT=/tmp/referenceAnalyzer-upstream-joern-smoke-runs
FIRMWARE_ROOT=/tmp/referenceAnalyzer-focused-luac

cd "$referenceAnalyzer"
python3 -m tools.real_firmware.OpenWrtDerived_compare init \
  --run-id "$RUN_ID" \
  --run-root "$RUN_ROOT" \
  --firmware-profile custom \
  --firmware-root "$FIRMWARE_ROOT" \
  --luabyte-result-dir "$RUN_ROOT/$RUN_ID-luabyte-placeholder" \
  --upstream-joern-clone "$JOERN_CLONE"

python3 -m tools.real_firmware.OpenWrtDerived_compare run-joern \
  --run-dir "$RUN_ROOT/$RUN_ID" \
  --upstream-joern-clone "$JOERN_CLONE" \
  --joern-command "$JOERN_COMMAND"

python3 - <<'PY'
from pathlib import Path
from tools.real_firmware.normalize_joern import normalize_joern_run

run_dir = Path("/tmp/referenceAnalyzer-upstream-joern-smoke-runs/joern-upstream-smoke")
normalize_joern_run(run_dir=run_dir, run_id=run_dir.name)
PY
```

Expected output:

- `raw/joern/run-errors.json` contains `{"errors": []}` when the staged command
  can process the selected material.
- `raw/joern/command-record.json` records `target_kind=upstream-clone` for this
  clone.
- `normalized/joern.jsonl` is present.

The benchmark adapter smoke is controlled by referenceAnalyzer because referenceAnalyzer owns the
oracle, firmware selection, normalization, comparison, and performance
judgment. It proves that this Joern clone can produce upstream-clone output for
the controller. It is not a standalone full-corpus benchmark and does not claim
QueryDB inclusion, schema extension acceptance, distribution acceptance, source
parser AST support, production security-query readiness, or official frontend
acceptance.
