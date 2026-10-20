Joern unit tests
================

Every module has unit tests under `{module}/src/test/scala`. The test framework is [ScalaTest](https://www.scalatest.org)
(3.2.x), with the `AnyWordSpec` style and `Matchers`.

Running tests
-------------

Always scope the run to the module you work on. All commands can be run in an interactive `sbt` shell, or as
`sbt "<command>"` from the command line.

```bash
sbt c2cpg/test                                      # all tests of one module
sbt "c2cpg/testOnly *MethodTests"                   # one test class
sbt "c2cpg/testOnly *MethodTests -- -z parameters"  # tests whose name contains "parameters"
sbt c2cpg/testFull                                  # force a full run
sbt testFull                                        # everything, this is what CI runs
```

- sbt 2.x runs tests incrementally: it skips tests that already passed on unchanged inputs.
  Use `testFull` to force a full run, for example to reproduce CI.
- Tests run in a forked JVM (`Test / fork := true`), in parallel.
- Test names must be unique within a class.
- The root `test` task aggregates all modules except `integrationTests` and `linterRulesInput`,
  see [integration tests](05_integration_tests.md).

Where tests live
----------------

```
{module}/src/test/scala/io/joern/{module}/{area}/{Name}Tests.scala
{module}/src/test/resources/...        test inputs that are too large for an inline string
```

- Name the class `{Something}Tests`.
- Put a test next to the tests of the same area. For a frontend those are typically `passes/`, `querying/`,
  `dataflow/` or `astcreation/`. Look at the existing layout of the frontend first.

Frontend tests
--------------

Frontend tests translate a code snippet to a CPG, and then query the CPG. Every frontend has a fixture class in
its `src/test/scala/.../testfixtures/` package. Derive your test from it, and use `code("...")` to build the CPG:

```scala
package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MethodTests extends C2CpgSuite {

  "MethodTest1" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |}""".stripMargin)

    "contain exactly one method node with the correct fields" in {
      inside(cpg.method.name("main").l) { case List(method) =>
        method.name shouldBe "main"
        method.fullName shouldBe "main"
        method.signature shouldBe "int(int,char**)"
        method.filename shouldBe "Test0.c"
      }
    }
  }
}
```

- `code(snippet)` and `code(snippet, fileName)` create a test CPG. The CPG is closed after the suite finished.
- Prefer one `cpg` per `should` block, so that the cost of running the frontend is shared by the test cases.
- Use `inside(...) { case ... }` to pattern match on results. This gives a better failure message than `.head`.
- Use `.l` to materialize a traversal into a `List`.
- Assert on the whole result instead of only on its size, where practical.
- Write the code snippet inline with `stripMargin`. Use `src/test/resources` for larger inputs.

| Module | Fixture to extend |
|--------|-------------------|
| `x2cpg` | `Code2CpgFixture` (base class of the frontend fixtures), `CfgTestFixture` |
| `c2cpg` | `C2CpgSuite`, `AstC2CpgSuite`, `DataFlowCodeToCpgSuite` |
| `javasrc2cpg` | `JavaSrcCode2CpgFixture`, `JavaDataflowFixture` |
| `kotlin2cpg` | `KotlinCode2CpgFixture` |
| `jimple2cpg` | `JimpleCode2CpgFixture`, `JimpleDataFlowCodeToCpgSuite` |
| `jssrc2cpg` | `JsSrc2CpgSuite`, `DataFlowCodeToCpgSuite` |
| `pysrc2cpg` | `PySrc2CpgFixture` |
| `php2cpg` | `PhpCode2CpgFixture` |
| `rubysrc2cpg` | `RubyCode2CpgFixture` |
| `gosrc2cpg` | `GoCodeToCpgSuite` |
| `swiftsrc2cpg` | `SwiftSrc2CpgSuite`, `DataFlowCodeToCpgSuite` |
| `csharpsrc2cpg` | `CSharpCode2CpgFixture` |
| `rust2cpg` | `Rust2CpgSuite` |
| `abap2cpg` | `Abap2CpgSuite` |
| `ghidra2cpg` | `GhidraBinToCpgSuite`, `DataFlowBinToCpgSuite` |

The fixtures are the source of truth, and they change over time. If a table entry is out of date, fix it in your PR.

Data flow tests
---------------

Data flow tests are part of the frontend that is tested, but they run the data flow engine of
`dataflowengineoss`. Either use the frontend's data flow fixture (for example `DataFlowCodeToCpgSuite`), or pass
`withOssDataflow = true` to a fixture that supports it, like `C2CpgSuite(withOssDataflow = true)`.

```scala
import io.joern.dataflowengineoss.language.*

val source = cpg.identifier.name("a")
val sink   = cpg.call.name("free").argument(1)
sink.reachableByFlows(source).l.size shouldBe 1
```

If you need custom semantics (`FlowSemantic`), pass them to the fixture's `semantics` argument.

Tests for `semanticcpg`, `dataflowengineoss`, `console` and `querydb`
---------------------------------------------------------------------

- `semanticcpg`: tests test the traversal steps on small hand-built graphs. `MockCpg` (in `io.shiftleft.semanticcpg.testing`) helps to build them.
- `dataflowengineoss`: tests use the `SemanticCpgTestFixture` trait, and the fixtures of `x2cpg` (test classpath dependency).
- `console`: tests use `ConsoleFixture` to create a temporary workspace.
- `querydb`: each query bundle has a test class that extends one of the `*QueryTestSuite` classes in
  `io.joern.suites`, for example `CQueryTestSuite(CopyLoops)`. See [querydb/README.md](../querydb/README.md).

Tests that need the network
---------------------------

Mark tests that download something with the `NetworkTest` tag (`io.joern.x2cpg.testfixtures.NetworkTest`):

```scala
"fetch the dependency" taggedAs NetworkTest in { ... }
```

For flaky or asynchronous assertions use `RetryableAssertion.eventually` instead of `Thread.sleep`.

What a good test looks like
---------------------------

- It tests one behavior. The `in` text says what the expected behavior is.
- For a bug fix, the test fails without the fix. Confirm that before you push.
- It does not depend on the machine: no absolute paths, no environment variables, no installed tools that CI does
  not have. The test must pass on Linux, macOS and Windows.
- It does not depend on iteration order of unordered collections. Sort, or compare as a `Set`.
