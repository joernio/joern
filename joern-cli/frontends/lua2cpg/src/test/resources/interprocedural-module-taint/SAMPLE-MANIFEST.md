# E4 Interprocedural Module Taint Samples

Source family: committed Lua 5.1 bytecode fixtures covering interprocedural
module and taint behavior.

Positive samples:
- d16-rf-interprocedural-formvalue-execute/input.luac: resolved same-artifact arg/return.
- d16-rf-webcmd-cross-module-popen/controller.luac and mtkwifi.luac: require/module/export/cross-module path.
- d24-module-return-table-field-call/controller.luac and library.luac: returned table field call target.
- bc-taint-minimal-path/input.luac: minimal same-artifact taint path.
- table-field-provenance-positive/input.luac: same fixed table key preserves taint provenance.
- conditional-merge-positive/input.luac: an optional overwrite preserves the bypassed source at branch merge.
- control-selection-positive/input.luac: a tainted predicate controls selection of a value consumed by a sink.

Negative samples:
- d24-interproc-unresolved-callee-negative/input.luac: unresolved callee boundary.
- d24-module-ambiguous-unresolved-dynamic-negative/*.luac: missing, ambiguous, and dynamic require boundaries.
- d24-module-missing-field-negative/*.luac: missing export field boundary.
- bc-kill-overwrite/input.luac and bc-branch-negative/input.luac: killed/no-flow taint boundaries.
- table-field-provenance-negative/input.luac: distinct fixed table keys do not share taint provenance.
- nested-branch-overwrite-negative/input.luac: an entered branch overwrite kills the prior source before its sink.
- control-unrelated-negative/input.luac: a predicate does not taint assignments outside its controlled region.
- control-overwrite-negative/input.luac: a post-merge overwrite kills prior control-dependent selection.

Reviewer command:
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.InterproceduralModuleTaintSmokeTest'
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/stage'
git status --short

This subset is sufficient for reviewer smoke of E4 semantics and is consumed
entirely from the `lua2cpg` test resources.
