AGENTS instructions
===================

Joern is a Scala 3 platform that turns source code, bytecode and binaries into Code Property Graphs (CPGs)
and queries them. The main developer documentation is the [Contributor's guide](contributing-docs/README.md).

- [Architecture and module layout](contributing-docs/01_architecture.md)
- [Development environment](contributing-docs/02_development_environment.md)

Compiling with sbt
------------------

Always scope to the module you are working on. Compiling or testing the whole build is slow.

```bash
sbt --client --color=false --supershell=false --batch c2cpg/compile
sbt --client --color=false --supershell=false --batch "c2cpg/Test/compile"
```

Module names are the `lazy val`s at the top of [build.sbt](build.sbt), for example `semanticcpg`, `x2cpg`,
`dataflowengineoss`, `c2cpg`, `javasrc2cpg`, `console`, `joerncli`, `querydb`.

The build uses `-Werror`: a compiler warning fails the build.

Pull request guideline
----------------------

- Follow [contributing-docs/08_pull_requests.md](contributing-docs/08_pull_requests.md).
- [ ] Title is `[module] Description`, for example `[javasrc2cpg] Fix addition operator`.
- [ ] Before working on a bug fix, **reproduce the problem** with a failing test.
- [ ] After making the change, confirm that **it compiles, the new test passes, and the problem is fixed**.
- [ ] Format and lint before submitting (see below).
- Write a short PR description and link the related issue, if any.

Coding style
------------

```bash
sbt --client --color=false --supershell=false --batch "scalafmt ; Test/scalafmt ; integrationTests/scalafmt ; integrationTests/Test/scalafmt"
sbt --client --color=false --supershell=false --batch "scalafix --diff-base origin/master RestrictedImports SingleLetterIdentifiers UnorderedIteration"
```

- Follow [Coding style and best practices](contributing-docs/03_coding_style.md).
- Scala 3 with **braces**: the build uses `-no-indent -old-syntax`. Do NOT use indentation-based syntax,
  `end` markers or `:` fewer-braces lambdas.
- Max line length is 120 (scalafmt).
- Do not use `better.files` or `scala.sys.process`. Use `io.shiftleft.semanticcpg.utils.FileUtil`, `os-lib`,
  `java.nio.file` and `io.shiftleft.semanticcpg.utils.ExternalCommand`.
- No single-letter identifiers except `i`, `j`, `k` and `_`.
- Do not iterate over unordered collections (`HashMap`, `HashSet`, `TrieMap`, ...) where the order can reach the CPG.
  CPG output must be deterministic. Sort first, or use an ordered collection.
- Code must work on Linux, macOS and Windows. CI runs on all three.
- Avoid inline comments that restate the code. Document intent with ScalaDoc.

Tests
-----

Always add a unit test that verifies your change.

- [contributing-docs/04_unit_tests.md](contributing-docs/04_unit_tests.md)
- [contributing-docs/05_integration_tests.md](contributing-docs/05_integration_tests.md)
- [contributing-docs/06_manual_tests.md](contributing-docs/06_manual_tests.md)

Tests live in `{module}/src/test/scala/io/joern/{module}/...` and are named `{Name}Tests.scala`.
Frontend tests derive from the frontend's own fixture (for example `C2CpgSuite`) and use `code("...")` to build a CPG.

For example, here's how to run a single test suite:

```bash
sbt --client --color=false --supershell=false --batch "c2cpg/testOnly *MethodTests"
```

To run all tests of one module:

```bash
sbt --client --color=false --supershell=false --batch c2cpg/test
```

sbt 2.x skips tests that already passed on unchanged inputs. Use `testFull` (for example `c2cpg/testFull`) to
force a full run, which is what CI does.

Several frontends need external tools (Go, Swift, Rust, PHP) to run their tests. See
[Development environment](contributing-docs/02_development_environment.md).

Architecture
------------

- [contributing-docs/01_architecture.md](contributing-docs/01_architecture.md)
- [contributing-docs/07_tech_stack.md](contributing-docs/07_tech_stack.md)
- [contributing-docs/09_common_tasks.md](contributing-docs/09_common_tasks.md)

Dependency direction is `semanticcpg` -> `x2cpg` -> `dataflowengineoss` and `x2cpg` -> frontends,
then `console` -> `joern-cli` -> `querydb`. Never introduce a dependency in the other direction.
A change in `semanticcpg` or `x2cpg` affects every frontend, so run the tests of the dependent modules too.

The CPG schema (node and edge types) is NOT defined in this repository. It comes from the external
`codepropertygraph` library, whose version is `cpgVersion` in [build.sbt](build.sbt).

Dependencies and build files
----------------------------

- Dependency versions are declared in [project/Versions.scala](project/Versions.scala).
- The experimental Bazel build ([bazel.md](bazel.md)) mirrors part of the sbt build. When you add or change a
  dependency or a source layout in a module that has a `BUILD` file, update `BUILD` and `MODULE.bazel` too.
- NEVER edit `maven_install.json` by hand. Regenerate it with `REPIN=1 bazel run @maven//:pin`.
- NEVER commit downloaded astgen binaries or other generated artifacts. They are git-ignored.

Copyright
---------

- NEVER reproduce copyrighted material. Test code must be written from scratch or be under a compatible license.
