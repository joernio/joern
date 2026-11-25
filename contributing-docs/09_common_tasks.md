Common tasks
============

Step-by-step guides for changes that touch several files. Use the most recent similar commit in `git log` as a
reference, for example `git log --stat -- joern-cli/frontends/rust2cpg` for a new frontend.

Fix a frontend bug
------------------

1. Reproduce the problem with a small code snippet, see [manual tests](06_manual_tests.md).
2. Add a failing test to the frontend, next to the tests of the same language construct,
   see [unit tests](04_unit_tests.md).
3. Find the code that creates the wrong nodes. Look in `astcreation/` of the frontend first, then in `passes/`, and
   in the shared passes of `x2cpg` if the problem is not language-specific.
4. Fix it. Run the tests of the frontend (`sbt {frontend}/test`), and the tests of the modules that depend on
   anything you changed in `x2cpg`.
5. Open the PR as `[{frontend}] Description`, see [pull requests](08_pull_requests.md).

Report a non-fatal problem from a frontend
------------------------------------------

Problems that do not abort the run (a file that cannot be parsed, dependencies that cannot be fetched, a crashing
helper tool) go into the `ScanReport` of the run, so that downstream tools do not have to grep the logs. Users get the
report as JSON with `--report-json` (next to the CPG) or `--report-json-file <file>`. The format is described in the
[x2cpg README](../joern-cli/frontends/x2cpg/README.md#scan-report).

- `X2CpgFrontend.run` creates one `ScanReport` per run and reports it at the end. Frontends receive it by overriding
  `createCpg(config, report)`. Define `createCpg(config)` as `createCpg(config, new ScanReport())`.
- Pass the report to the passes that process files, see `c2cpg/passes/AstCreationPass`. Utilities that only need to
  report problems take a `ProblemSink` (a `ScanReport` is one).
- Replace the `logger.warn` or `logger.error` of the problem by
  `report.warn(category, message, file, cause, logger)` or `report.error(...)`. This logs exactly as before and records
  the problem. Use the method that matches the level of the log statement you replace: never change a log level when
  you migrate a call site. Use `report.record(Problem(...))` if the problem is already logged somewhere else, with the
  severity that matches that log statement.
- Pick the `Category` that fits: `FileParse`, `FileProcessing`, `FileSkipped`, `DependencyResolution`, `ExternalTool`
  or `Other`. The category names are part of the JSON format, do not rename them.
- Do not repeat the file in the `message`. Pass it as `file`, relative to the input path: it is a field of its own in
  the JSON report, and it is added in front of the message when the problem is logged.
- Fields that are not available (`file`, `exception`, the lines of code of an unreadable file, ...) are left out of the
  JSON report, they are never `null`. The `exception` is an object with the `type` (class name) and, if the exception has
  one, its `message`.
- Per-file results (lines of code, parsed, CPG created, duration) are recorded
  with `report.addReportInfo` and `report.updateReport`.
- Test it with a frontend whose input triggers the problem and assert on `report.allProblems`.

Add a CPG pass
--------------

A pass reads the CPG and adds nodes and edges through a `DiffGraphBuilder`. The passes of the base layers are in
[x2cpg/src/main/scala/io/joern/x2cpg/passes](../joern-cli/frontends/x2cpg/src/main/scala/io/joern/x2cpg/passes).

```scala
class NamespaceCreator(cpg: Cpg) extends CpgPass(cpg) {
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // read from `cpg`, write to `dstGraph`
  }
}
```

- Use `ForkJoinParallelCpgPass` when the work can be split into independent parts. Make sure the result stays
  deterministic, see [coding style](03_coding_style.md).
- A pass must not modify the graph while it reads it. Everything goes through `dstGraph`.
- Document the prerequisites of the pass (which other passes must have run) in its ScalaDoc.
- Register the pass where it belongs: in the frontend's `createCpg` for a frontend pass, or in a layer
  (`x2cpg/layers`) for a pass that all frontends share.
- Test it by creating a small CPG, applying the pass, and asserting on the result. See the existing tests in
  `x2cpg/src/test/scala/io/joern/x2cpg/passes`.

Add a query
-----------

Queries live in `querydb/src/main/scala/io/joern/scanners/{language}/`. Follow
[querydb/README.md](../querydb/README.md): annotate the query with `@q`, put it in a `QueryBundle` object, give every
parameter a default value, and add a test with the matching `*QueryTestSuite`, for example `CQueryTestSuite`.

```bash
sbt querydb/test
sbt "querydb/testOnly *CopyLoopTests"
```

Add a language frontend
-----------------------

The newest frontend, `rust2cpg`, is a good model. You need to touch:

| File | Change |
|------|--------|
| `joern-cli/frontends/{name}/` | The module: `build.sbt`, `src/main`, `src/test`, a launcher script `{name}.sh`, and a `README.md` |
| [project/Projects.scala](../project/Projects.scala) | Add `lazy val {name} = project.in(frontendsRoot / "{name}")` |
| [build.sbt](../build.sbt) | Add `lazy val {name} = Projects.{name}` and add it to the `root` aggregate |
| [joern-cli/build.sbt](../joern-cli/build.sbt) | Add the `Universal / mappings` line with `frontendMappings(...)` |
| `joern-cli/src/universal/{name}` and `{name}.bat` | The launcher scripts for the distribution, copy them from another frontend |
| `console/.../cpgcreation/` | `{Name}CpgGenerator`, registration in `package.scala`, `CpgGeneratorFactory.scala` and `ImportCode.scala` |
| [tests/code](../tests/code) and [testDistro.py](../testDistro.py) | A small project for the distribution test, and its entry in `frontend_configs` |
| `{name}/BUILD`, [MODULE.bazel](../MODULE.bazel) | Optional: the Bazel build, see [bazel.md](../bazel.md) |
| [.github/workflows](../.github/workflows) | If the frontend needs a toolchain on the CI runners |

Then:

1. Extend `X2CpgFrontend` and `X2CpgMain`, run the `MetaDataPass` first, and create the AST with `AstCreatorBase`.
2. Add a test fixture class (extending `Code2CpgFixture`) in `src/test/scala/.../testfixtures`, see
   [unit tests](04_unit_tests.md).
3. Add the new fixtures to the table in [04_unit_tests.md](04_unit_tests.md), and the parser to the table in
   [07_tech_stack.md](07_tech_stack.md).

Update dependencies
-------------------

- **A library**: change the version in [project/Versions.scala](../project/Versions.scala). Dependencies that are only
  used by one module are declared in its `build.sbt`.
  Some versions are shared with the meta-build, see the comment in `Versions.scala`
  and [project/meta-build.sbt](../project/meta-build.sbt).
- **The Bazel build**: update [MODULE.bazel](../MODULE.bazel), and re-pin the lock file with
  `REPIN=1 bazel run @maven//:pin`. Do not edit `maven_install.json` by hand.
- **codepropertygraph** (`cpgVersion` in [build.sbt](../build.sbt)): run `./updateDependencies.sh`, or edit it by
  hand. Bazel pins a git commit, `CODEPROPERTYGRAPH_VERSION` in [MODULE.bazel](../MODULE.bazel).
- **An AST generator**: change the version in the frontend's `src/main/resources/application.conf`. Bazel keeps its
  own copy for some of them, for example `JSSRC2CPG_ASTGEN_VERSION` in [MODULE.bazel](../MODULE.bazel).
- **Dependabot** updates the GitHub Actions and Docker images, see [.github/dependabot.yml](../.github/dependabot.yml).
  Some libraries are excluded from automatic updates, see [.scala-steward.conf](../.scala-steward.conf).
  A [nightly workflow](../.github/workflows/upgrade-deps.yml) checks that the latest upstream dependencies still work.

After an update, run the tests of every module that uses the dependency.

Update the Scala or sbt version
-------------------------------

- **sbt**: [project/build.properties](../project/build.properties) and
  `joern-cli/src/universal/schema-extender/project/build.properties`.
- **Scala**: the version appears in several places. The best reference is the commit that did the last bump,
  `git log --grep 'Bump Scala'`. It changed `build.sbt`, `joern-cli/src/universal/schema-extender/build.sbt`,
  `linter-rules/build.sbt` (the scalafix testkit artifact is published per Scala version), `MODULE.bazel` (including
  the `scala-repl-pp` artifact), `bazel/scala.bzl`, `linter-rules/BUILD`, and the Bazel lock files.
  `codepropertygraph` and `scala-repl-pp` must be published for the new Scala version first.

Add a script
------------

Ready-to-run scripts are in [scripts/](../scripts), see the [scripts README](../scripts/README.md). A script that
the distribution test should run is added to `script_configs` in [testDistro.py](../testDistro.py).
