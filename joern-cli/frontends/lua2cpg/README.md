# lua2cpg

`lua2cpg` is a Lua 5.1 analyzer for Joern. It builds Code Property
Graphs for Lua programs through a bytecode-based pipeline and emits
taint-analysis evidence for Lua code, including interprocedural flow,
source/sink matches, sanitizer classifications, and vulnerability reports.

The analysis core operates on Lua 5.1 bytecode. To analyze Lua source files,
compile them with `luac5.1` and pass the generated `.luac` files to
`lua2cpg`. Source files placed in the input tree are also recorded in the CPG
file inventory.

## Prerequisites

- Use the JDK and `sbt` versions required by the Joern repository.
- Run the commands below from the Joern repository root.
- Install `luac5.1` when starting from Lua source files.

## Build

Build the staged frontend command:

```bash
sbt 'lua2cpg/stage'
```

The staged command is written to:

```bash
joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg
```

Check the available options:

```bash
joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg --help
```

## Quickstart

Analyze an existing Lua 5.1 bytecode directory:

```bash
LUA2CPG=joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg
INPUT=/path/to/lua-bytecode
OUTPUT=/tmp/lua.cpg.bin

"$LUA2CPG" "$INPUT" --output "$OUTPUT"
```

Analyze a Lua source file by compiling it to Lua 5.1 bytecode first:

```bash
mkdir -p /tmp/lua-bytecode
luac5.1 -o /tmp/lua-bytecode/app.luac /path/to/app.lua

joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg \
  /tmp/lua-bytecode \
  --output /tmp/lua.cpg.bin
```

For a source tree, preserve the directory layout while compiling:

```bash
SRC_ROOT=/path/to/lua-source
BC_ROOT=/tmp/lua-bytecode

mkdir -p "$BC_ROOT"
find "$SRC_ROOT" -name '*.lua' -print0 |
  while IFS= read -r -d '' file; do
    rel="${file#$SRC_ROOT/}"
    out="$BC_ROOT/${rel%.lua}.luac"
    mkdir -p "$(dirname "$out")"
    luac5.1 -o "$out" "$file"
  done

joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg \
  "$BC_ROOT" \
  --output /tmp/lua.cpg.bin
```

## Inspecting The CPG

Open the generated CPG with Joern and inspect the Lua-specific evidence:

```bash
joern /tmp/lua.cpg.bin
```

Useful traversals:

```scala
cpg.metaData.language.l
cpg.file.name.l
cpg.method.fullName.l
cpg.call.nameExact("lua.module.resolution").code.l
cpg.call.nameExact("lua.calltarget.candidate").code.l
cpg.call.nameExact("lua.source.endpoint").code.l
cpg.call.nameExact("lua.sink.endpoint").code.l
cpg.call.nameExact("lua.sanitizer.classification").code.l
cpg.call.nameExact("lua.report.vulnerability").code.l
```

## Taint Evidence Export

`lua2cpg` can also write reviewer-visible JSON evidence for benchmark and
debugging workflows:

```bash
joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg \
  /path/to/lua-bytecode \
  --output /tmp/lua.cpg.bin \
  --lua-real-firmware-output-dir /tmp/lua2cpg-evidence
```

The evidence directory contains:

- `decoder-summary.json`: input, decode, prototype, instruction, callsite, and
  local-flow totals.
- `path-search-profile.json`: taint path-search counters and retained pair
  profiles.
- `run-summary.json`: native run status and decode totals.
- `run-errors.json`: native run errors.
- `staging/*.json`: per-artifact decode, flow, module-resolution, source/sink,
  sanitizer, and path-evidence rows.

This export is optional. The primary `lua2cpg` output is the CPG written by
`--output`.

## Supported Analysis

- Lua version: Lua 5.1.
- Inputs: Lua 5.1 `.luac` bytecode; Lua source after compilation with
  `luac5.1`.
- CPG content: file inventory, bytecode artifacts, prototypes, constants,
  instructions, methods, identifiers, calls, and reaching definitions.
- Program analysis: intraprocedural value flow, module require/return-table
  linkage, cross-module call targets, interprocedural argument and return flow,
  source/sink matching, sanitizer classification, and vulnerability reports.
- Distribution boundary: this frontend does not ship QueryDB queries as part
  of this README. Reviewer-visible results are exposed through CPG nodes and
  optional JSON evidence.

## Architecture

The frontend is organized around a bytecode-first analysis pipeline:

- `LuaFileInventoryPass` records `.lua` source files in the CPG file inventory.
- `LuaBytecodeDecoder` decodes Lua 5.1 bytecode artifacts into profiles,
  prototypes, constants, instructions, and diagnostics.
- `LuaBytecodeModelPass` emits the decoded bytecode model into the CPG.
- `LuaInstructionSemantics` computes prototype-local value, call, table,
  global, upvalue, and boundary facts.
- `LuaProgramSemantics` normalizes module-level and interprocedural flow,
  source/sink, sanitizer, and report evidence across decoded artifacts.
- `LuaRealFirmwareEvidenceExporter` writes the optional JSON evidence directory
  requested by `--lua-real-firmware-output-dir`.

## Tests

Use the smallest tier that answers the review question first.

Quick smoke:

```bash
sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'
sbt 'lua2cpg/stage'
joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg --help
git status --short
```

Focused capability tests:

```bash
sbt 'lua2cpg/testOnly io.joern.lua2cpg.BytecodeModelSmokeTest'
sbt 'lua2cpg/testOnly io.joern.lua2cpg.IntraproceduralSemanticsSmokeTest'
sbt 'lua2cpg/testOnly io.joern.lua2cpg.InterproceduralModuleTaintSmokeTest'
sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'
sbt 'lua2cpg/testOnly io.joern.lua2cpg.RealFirmwareEvidenceExportSmokeTest'
```

Full frontend test suite:

```bash
sbt 'lua2cpg/test'
```

The full `lua2cpg/test` suite is intentionally broader and can take about an
hour in this development environment.

## Optional External Benchmarking

referenceAnalyzer can run larger firmware benchmarks against a staged `lua2cpg`
command. This is optional external acceptance tooling: referenceAnalyzer owns firmware
selection, normalization, oracle comparison, and full-corpus benchmark
judgments. It is not required to build `lua2cpg`, generate a CPG, inspect the
CPG in Joern, or emit native JSON evidence.

Example adapter flow:

```bash
referenceAnalyzer=/path/to/referenceAnalyzer
JOERN_CLONE=$(pwd)
JOERN_COMMAND="$JOERN_CLONE/joern-cli/frontends/lua2cpg/target/universal/stage/bin/lua2cpg"
RUN_ID=joern-upstream-smoke
RUN_ROOT=/tmp/referenceAnalyzer-upstream-joern-smoke-runs
FIRMWARE_ROOT=/path/to/lua-bytecode-corpus

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
```
