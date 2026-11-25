x2cpg
=====

Base library of all Joern frontends: `X2CpgFrontend`, `X2CpgMain`, the shared passes and layers, the AST generator
helpers and the command line options that every frontend understands. See the
[architecture](../../../contributing-docs/01_architecture.md) for how it fits into the rest of Joern.

Scan report
-----------

A frontend does not abort on every problem. A file that cannot be parsed, dependencies that cannot be fetched or an
AST generator that fails for one file are logged, and the run continues with what is left. The **scan report** lists
these problems, together with the outcome per file, so that downstream tools do not have to grep the logs.

At the end of a run, every frontend logs the report: a table with one row per processed file (lines of code, whether
it was parsed, whether it got a CPG, and how long that took), followed by a summary of the problems.

Additionally, the report can be written as a JSON file:

```
  --report-json            additionally write the scan report (processed files and non-fatal problems) as JSON next to the CPG
  --report-json-file <file>
                           like --report-json, but write the JSON report to <file>
```

Without `--report-json-file`, the report is written next to the CPG, as `<output>.report.json`. For
`--output /path/to/cpg.bin` that is `/path/to/cpg.bin.report.json`. If no `--output` is given, no CPG is written to
disk, and the report is written to `cpg.bin.report.json` in the working directory. The report is written even if the
run fails, then with the status `Failed`. Failing to write the report only logs a warning and never fails the run.

### Format

```json
{
  "schemaVersion": 1,
  "frontend": "JsSrc2Cpg",
  "joernVersion": "v4.0.692",
  "inputPath": "/path/to/project",
  "cpgPath": "/path/to/cpg.bin",
  "status": "Completed",
  "summary": {
    "files": 375,
    "parsed": 371,
    "cpgGenerated": 371,
    "loc": 89537,
    "errors": 0,
    "warnings": 4
  },
  "problems": [
    {
      "severity": "Warning",
      "category": "FileParse",
      "file": "lib/broken_function.js",
      "message": "Failed to parse: Unexpected token (1:9)"
    },
    {
      "severity": "Warning",
      "category": "DependencyResolution",
      "message": "Could not fetch the dependencies of the project",
      "exception": { "type": "java.io.IOException", "message": "Connection refused" }
    }
  ],
  "files": [
    { "path": "lib/broken_function.js", "loc": 3, "parsed": false, "cpgGenerated": false },
    { "path": "server.ts", "loc": 645, "parsed": true, "cpgGenerated": true, "durationMs": 79.301 }
  ]
}
```

| Field | Description |
|-------|-------------|
| `schemaVersion` | Version of this format. Currently `1`. |
| `frontend` | Name of the frontend, e.g., `C2Cpg`. |
| `joernVersion` | Version of the frontend, or `local build`. |
| `inputPath` | The analyzed project, as an absolute path. |
| `cpgPath` | Where the CPG was written. Left out if it was not written to disk. |
| `status` | `Completed`, or `Failed` if the run ended with an exception. |
| `summary` | Totals over `files` and `problems`. `loc` only counts files that could be read. |
| `problems` | All non-fatal problems, see below. |
| `files` | One entry per file the frontend analyzed, sorted by `path`, see below. |

A **problem** has the following fields:

| Field | Description |
|-------|-------------|
| `severity` | `Warning` or `Error`. This is the level the problem is logged at. |
| `category` | What kind of problem it is, see below. |
| `file` | The affected file, relative to the input path where possible. Left out if the problem is not about one file. |
| `message` | What went wrong. It does not repeat `file`. |
| `exception` | The cause, if there is one: its class name as `type`, and its `message` if it has one. |

The `category` is one of:

| Category | Meaning |
|----------|---------|
| `FileParse` | A source file could not be parsed, or only with problems, e.g., by the AST generator. |
| `FileProcessing` | A parsed source file could not be turned into a CPG. |
| `DependencyResolution` | Dependencies of the analyzed project could not be resolved or fetched. |
| `ExternalTool` | An external tool (AST generator, compiler, package manager, ...) failed. |
| `Other` | Anything else, e.g., an input without any files to analyze. |

A **file** has the following fields:

| Field | Description |
|-------|-------------|
| `path` | The file, relative to the input path where possible. |
| `loc` | Number of lines. Left out if the file could not be read. |
| `parsed` | Whether the file was parsed. |
| `cpgGenerated` | Whether the frontend created nodes for the file. |
| `durationMs` | Time spent on the file in milliseconds. Left out if it was not measured. |

Some things to rely on, or not to rely on:

- Fields that are not available are always **left out**, they are never `null`.
- `problems` and `files` are sorted, so the same input yields the same report. Only `durationMs` varies.
- The category names are stable. New categories, fields and frontends may be added, so ignore what you do not know.
- Which problems a frontend reports differs. A missing problem is no guarantee that everything was fine.
- The `message` is meant for people. Do not parse it.
- Every frontend except `ghidra2cpg`, which does not report yet, lists the files it analyzed. A file that a
  `FileParse` or `FileProcessing` problem is about is listed too, as not parsed or as without a CPG.
- A file that could not be processed never has `cpgGenerated` set, even if its frontend recovered with an empty AST.
- `jimple2cpg` lists the classes: the `path` is the path of the class inside the archive or directory (the name of
  the class for dex files), and `loc` is only there if the class was decompiled.

To report problems from a frontend, see
[Report a non-fatal problem from a frontend](../../../contributing-docs/09_common_tasks.md#report-a-non-fatal-problem-from-a-frontend).
