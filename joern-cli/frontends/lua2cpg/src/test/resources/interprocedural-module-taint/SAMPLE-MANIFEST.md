# E4 Interprocedural Module Taint Samples

Source family: referenceAnalyzer shared E4 fixtures derived from OpenWrtDerived LuCI behavior.

Positive samples:
- d16-rf-interprocedural-formvalue-execute/input.luac: resolved same-artifact arg/return.
- d16-rf-webcmd-cross-module-popen/controller.luac and mtkwifi.luac: require/module/export/cross-module path.
- d24-module-return-table-field-call/controller.luac and library.luac: returned table field call target.
- bc-taint-minimal-path/input.luac: minimal same-artifact taint path.

Negative samples:
- d24-interproc-unresolved-callee-negative/input.luac: unresolved callee boundary.
- d24-module-ambiguous-unresolved-dynamic-negative/*.luac: missing, ambiguous, and dynamic require boundaries.
- d24-module-missing-field-negative/*.luac: missing export field boundary.
- bc-kill-overwrite/input.luac and bc-branch-negative/input.luac: killed/no-flow taint boundaries.

Reviewer command:
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.InterproceduralModuleTaintSmokeTest'
JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/stage'
git status --short

This subset is sufficient for reviewer smoke of E4 semantics. Full closure still depends on referenceAnalyzer controller evidence and phase-subset performance records.
