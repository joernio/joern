# Lua2Cpg Rules, Sanitizer, And Report Samples

These committed bytecode samples support the `RulesSanitizerReportSmokeTest`
reviewer smoke. They are consumed entirely inside the `lua2cpg` test resources
and provide focused Lua 5.1 bytecode coverage for source/sink matching,
sanitizer classification, report construction, and negative taint boundaries.

| Fixture | Fixture role | Capability reason | Consuming reviewer command |
| --- | --- | --- | --- |
| `bc-taint-minimal-path/input.luac` | Focused committed fixture | Minimal source-to-sink path for rule, endpoint, report, and report-boundary smoke coverage. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d16-rf-formvalue-os-execute-chain/input.luac` | Focused committed fixture | Final-segment `*.formvalue` source and `*.execute` sink positive with a constructed report. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d16-rf-submit-dpp-uri-execute/input.luac` | Focused committed fixture | Independent same-module formvalue-to-execute report positive. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d16-rf-webcmd-cross-module-popen/controller.luac` | Focused committed fixture | Cross-module source side for final-segment source/sink and report construction. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d16-rf-webcmd-cross-module-popen/mtkwifi.luac` | Focused committed fixture | Cross-module `*.popen` sink side for final-segment sink and report construction. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d24-sanitizer-suppresses-report/input.luac` | Focused committed fixture | On-chain sanitizer positive; emits sanitized classification and suppresses true-positive vulnerability reporting. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d24-rules-overmatch-constant-sink-negative/input.luac` | Focused committed fixture | Rejects `formvaluex`, `executex`, and fixed-string sink arguments. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d24-sanitizer-same-suffix-off-chain-negative/input.luac` | Focused committed fixture | Same-suffix sanitizer call not on the path remains `not-sanitized` and does not suppress the report. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `d24-report-no-report-without-path-negative/input.luac` | Focused committed fixture | Endpoint-only source/sink evidence does not create a vulnerability report. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `bc-kill-overwrite/input.luac` | Focused committed fixture | Killed taint path negative boundary; no vulnerability report should be emitted. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
| `bc-branch-negative/input.luac` | Focused committed fixture | Branch-negative no-path boundary; no vulnerability report should be emitted. | `JAVA_TOOL_OPTIONS='-Dsbt.watch.mode=polling -Dsbt.io.jdktimestamps=true' sbt 'lua2cpg/testOnly io.joern.lua2cpg.RulesSanitizerReportSmokeTest'` |
