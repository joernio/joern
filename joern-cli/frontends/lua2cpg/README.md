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
