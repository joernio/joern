# Lua2Cpg OpenWrt-Derived Firmware Lua Fixture Corpus

This committed fixture corpus contains Lua files extracted from the
`usr/lib/lua` tree of a real-device OpenWrt-derived firmware image. It is used
by `lua2cpg` tests as a self-contained reviewer corpus for Lua 5.1 source file
inventory, Lua 5.1 bytecode decoding, CPG generation, and native taint evidence
export.

The outer resource name and test descriptions are vendor-neutral. The original
relative paths and file bytes below `usr/lib/lua` are preserved.

## Contents

- Scope: `usr/lib/lua`
- Lua source files: 42
- Lua bytecode files: 42
- Total `.lua` and `.luac` files: 84

## Notes

- The `.lua` files exercise source file inventory in the CPG.
- The `.luac` files exercise the bytecode-based semantic and taint-analysis
  pipeline.
- This corpus is consumed entirely from `lua2cpg` test resources.
- This manifest does not assert licensing or redistribution facts beyond the
  presence of the committed test files.
