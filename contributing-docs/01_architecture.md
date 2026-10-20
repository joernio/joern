Architecture
============

Joern turns code into a Code Property Graph (CPG) and lets you query it.
The pipeline is: a **frontend** parses code and creates the initial graph, **passes** enrich it
(call graph, control flow, type recovery, ...), and the **console** or **query database** query it.

Modules
-------

The modules are the sbt projects listed at the top of [build.sbt](../build.sbt) and defined in
[project/Projects.scala](../project/Projects.scala).

| Module | Path | Purpose |
|--------|------|---------|
| `semanticcpg` | `semanticcpg/` | Extensions on the generated CPG model: the traversal DSL (`io.shiftleft.semanticcpg.language`), overlays and layers, dot and code dumpers, SARIF, and utilities such as `FileUtil` and `ExternalCommand`. Foundation for all other modules. |
| `macros` | `macros/` | Scala macros used by the console (compile-time code generation for traversal steps). |
| `x2cpg` | `joern-cli/frontends/x2cpg/` | Base library for all frontends: `X2CpgFrontend`, `X2CpgMain`, `AstCreatorBase`, shared passes, layers, type stubs, astgen helpers. Its test fixtures are reused by every frontend. |
| `dataflowengineoss` | `dataflowengineoss/` | Open source data flow engine: reaching definitions, taint tracking, semantics, slicing. |
| Frontends | `joern-cli/frontends/*` | One module per language, see below. |
| `console` | `console/` | The interactive shell, scripting interface, workspace handling, and the `cpgcreation` package that invokes frontends. |
| `joerncli` | `joern-cli/` | The `joern`, `joern-parse`, `joern-scan`, `joern-export`, `joern-slice` and `joern-flow` executables. Packages all frontends and the console into the distribution. |
| `querydb` | `querydb/` | Security queries and the scanner behind `joern-scan`. |
| `linterRules` | `linter-rules/` | Custom scalafix rules, applied to all modules, see [coding style](03_coding_style.md). |
| `integrationTests` | `integration-tests/` | End-to-end tests, see [integration tests](05_integration_tests.md). |

Dependencies
------------

```
codepropertygraph (external: schema, generated node classes, flatgraph)
  └── semanticcpg
        ├── x2cpg ──────────── frontends (c2cpg, javasrc2cpg, ...)
        │     └── dataflowengineoss
        └── macros
console    = semanticcpg + macros + x2cpg
joerncli   = console + dataflowengineoss + x2cpg, and bundles every staged frontend
querydb    = console + macros + dataflowengineoss
```

- Frontends depend on `x2cpg`. They depend on `dataflowengineoss` only for tests.
- `console` depends on `c2cpg`, `jssrc2cpg` and `swiftsrc2cpg` for tests only.
- `kotlin2cpg` also depends on `javasrc2cpg`.
- Do not introduce dependencies in the other direction. A change in `semanticcpg` or `x2cpg` can affect everything.

The CPG schema
--------------

Node types, edge types, properties and the generated Scala classes are **not** defined in this repository.
They come from [codepropertygraph](https://github.com/ShiftLeftSecurity/codepropertygraph) and the
[CPG specification](https://cpg.joern.io). The version is `cpgVersion` in [build.sbt](../build.sbt).
If you need a schema change, it has to be made upstream first.

Anatomy of a frontend
---------------------

Take `c2cpg` (`joern-cli/frontends/c2cpg`) as an example:

- `Main.scala`: the command line entry point (extends `X2CpgMain`) and the `Config` (extends `X2CpgConfig`).
- `C2Cpg.scala`: extends `X2CpgFrontend`, and `createCpg` runs the passes in order.
- `astcreation/`: translates the language's parse tree into CPG AST nodes (extends `AstCreatorBase`).
- `parser/`: wraps the language's parser.
- `passes/`: frontend-specific passes. Each pass is a `CpgPass` or one of its parallel variants.
- `src/test/`: tests and the frontend's fixtures in `testfixtures/`.

Frontends differ in the parser they use, see [tech stack](07_tech_stack.md). `jssrc2cpg`, `gosrc2cpg`,
`csharpsrc2cpg`, `swiftsrc2cpg`, `rust2cpg` and `abap2cpg` rely on external AST generator ("astgen") binaries that
are downloaded at build time, and `php2cpg` downloads the `php-parser` phar.

How a frontend is found by the console
--------------------------------------

The `console` module has one `*CpgGenerator` per language in
`console/src/main/scala/io/joern/console/cpgcreation/`. `importCode` guesses the language and starts the matching
frontend executable. `joerncli` stages all frontends into the distribution, see `Universal / mappings` in
[joern-cli/build.sbt](../joern-cli/build.sbt).

Further reading
---------------

- [Joern documentation](https://docs.joern.io)
- [CPG specification](https://cpg.joern.io)
- [changelog/](../changelog) for the major architectural migrations (Scala 3, flatgraph).
- Frontend-specific notes: `joern-cli/frontends/*/README.md`, for example
  [c2cpg](../joern-cli/frontends/c2cpg/README.md).
