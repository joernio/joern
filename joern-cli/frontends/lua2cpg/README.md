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
