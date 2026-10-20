Tech stack
==========

This page explains some of the libraries and tooling used in the code base. Versions are in
[project/Versions.scala](../project/Versions.scala) and in the `build.sbt` of each module.

Testing
-------

See the previous sections for test libraries:

- [Unit tests](04_unit_tests.md)
- [Integration tests](05_integration_tests.md)

Language and build
------------------

- **Scala 3** (the version is `scalaVersion` in [build.sbt](../build.sbt)), compiled with `--release 17`, and run
  on JDK 17 or later. The code base is written in the `-no-indent -old-syntax` dialect, see
  [coding style](03_coding_style.md).
- **sbt 2.x** as the main build, see [project/build.properties](../project/build.properties). The sbt plugins
  are in [project/plugins.sbt](../project/plugins.sbt): `sbt-scalafmt`, `sbt-scalafix`, `sbt-native-packager`
  (packaging and `stage`), `sbt-dynver` (versions from git tags), `sbt-ci-release-early` (publishing) and `sbt-antlr4`.
- **Bazel** is an experimental secondary build, see [bazel.md](../bazel.md).
- **GitHub Actions** for CI, see [.github/workflows](../.github/workflows) and [pull requests](08_pull_requests.md).

Code property graph
-------------------

The graph model is provided by
[codepropertygraph](https://github.com/ShiftLeftSecurity/codepropertygraph), which is built on
[flatgraph](https://github.com/joernio/flatgraph), an in-memory columnar graph database that replaced overflowdb
in Joern 4.0 (see [changelog/4.0.0-flatgraph.md](../changelog/4.0.0-flatgraph.md)).

- `codepropertygraph` generates the node, edge and property classes from the schema. It also provides the base
  of `CpgPass` and the overlays.
- `semanticcpg` adds the traversal DSL on top, the `io.shiftleft.semanticcpg.language` package.
  Importing `io.shiftleft.semanticcpg.language.*` brings the steps into scope.
- The specification of the schema is published at https://cpg.joern.io.

Frontends and their parsers
---------------------------

| Frontend | Language | Parser |
|----------|----------|--------|
| `c2cpg` | C, C++ | Eclipse CDT (`io.joern:eclipse-cdt-core`, re-published by us, see the [c2cpg README](../joern-cli/frontends/c2cpg/README.md)) |
| `javasrc2cpg` | Java source | JavaParser with its symbol solver, Gradle Tooling API for dependencies, Lombok support |
| `kotlin2cpg` | Kotlin | `kotlin-compiler-embeddable` |
| `jimple2cpg` | JVM bytecode | Soot (Jimple), CFR to decompile |
| `jssrc2cpg` | JavaScript, TypeScript | external `astgen` binary |
| `pysrc2cpg` | Python | JavaCC grammar (`pythonGrammar.jj`), generated at build time |
| `php2cpg` | PHP | `php-parser` phar run with the system `php` |
| `rubysrc2cpg` | Ruby | `ruby_ast_gen`, executed through JRuby |
| `gosrc2cpg` | Go | external `goastgen` binary |
| `swiftsrc2cpg` | Swift | external `SwiftAstGen` binary |
| `csharpsrc2cpg` | C# | external `dotnetastgen` binary |
| `rust2cpg` | Rust | external `rust_ast_gen` binary |
| `abap2cpg` | ABAP | external `abapgen` binary |
| `ghidra2cpg` | Binaries | Ghidra (`io.joern:ghidra`) |

The external AST generators are maintained in
[joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo) and are downloaded by the build, see
[development environment](02_development_environment.md).

Data flow
---------

`dataflowengineoss` implements the open source data flow engine: reaching definitions on the CFG, and a
backwards-resolving taint query engine (`reachableBy` and `reachableByFlows`). Behavior of library calls is
described by semantics files, which are parsed with an [ANTLR](https://www.antlr.org) grammar
(`sbt-antlr4`). `DefaultSemantics` has the built-in semantics.

Console and scripting
---------------------

- [scala-repl-pp](https://github.com/mpollmeier/scala-repl-pp) is the Scala REPL and script runner behind the
  console, including `--script` and server mode.
- `cask` serves the HTTP API of the server mode (`joern --server`).
- `scopt` parses the command line of the tools and frontends.
- `upickle`/`ujson` is used for JSON.
- `macros` provides the compile-time helpers for the console.

Packaging
---------

`sbt joerncli/stage` and `sbt joerncli/Universal/packageBin` build the distribution with `sbt-native-packager`.
`createDistribution` creates the per-platform zips, see the `Releases` section in the [README](../README.md).
Docker images are built from [Dockerfile](../Dockerfile) and [ci/](../ci).

Linting
-------

[Scalafmt](https://scalameta.org/scalafmt/) and [Scalafix](https://scalacenter.github.io/scalafix/) with our own
rules in [linter-rules](../linter-rules). See [coding style](03_coding_style.md).
