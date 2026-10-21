Joern integration tests
=======================

Unit tests ([previous section](04_unit_tests.md)) run a frontend or a query in-process. Integration tests check that the
pieces work together as a user would use them: through the console, the scripts, and the packaged distribution.
CI runs both kinds on Linux, macOS and Windows.

There are three levels:

| Level | What | Where | How to run |
|-------|------|-------|------------|
| Unit tests | Frontends, passes, traversals, queries | `{module}/src/test` | `sbt {module}/test` |
| Integration tests | The console and scripts, driven through the real launchers | [integration-tests/](../integration-tests) | `sbt "c2cpg/stage ; integrationTests/test"` |
| Distribution test | The packaged `joern-cli` distribution and `querydb`, driven through the shell scripts | [testDistro.py](../testDistro.py) and [tests/](../tests) | see below |

Integration tests (`integrationTests`)
--------------------------------------

The sbt project `integrationTests` needs staged frontends, so it is deliberately **not** part of the root
aggregate. `sbt test` and `sbt testFull` do not run it. Run it explicitly:

```bash
sbt "c2cpg/stage ; integrationTests/test"
sbt "c2cpg/stage ; integrationTests/testOnly *RunScriptTests"
```

CI uses `integrationTests/testFull` to force a full run.

- The tests run in the sbt JVM (`Test / fork := false`), because scripts are compiled in-process with the inherited
  classpath.
- Test inputs are in [tests/code](../tests/code) and shared with the distribution test.
- Add a test here when you change the console, script handling, or anything that only breaks when the real
  launchers are used. Do not use it for frontend behavior, use a frontend unit test.

Distribution test (`testDistro.py`)
-----------------------------------

[testDistro.py](../testDistro.py) runs the `joern`, `joern-scan`, `joern-slice` executables against the staged
distribution. It covers the console, every frontend on a small project from [tests/code](../tests/code),
the scripts in [scripts/](../scripts), `joern-scan` with the query database, slicing, SARIF output, and the schema
extender.

```bash
python -m pip install pexpect          # wexpect on Windows
sbt joerncli/stage querydb/createDistribution
python -u ./testDistro.py
```

The `.sc` files in [tests/](../tests) are the Joern scripts used by the distribution test. Run them manually with
the staged console:

```bash
joern-cli/target/universal/stage/joern --script tests/test-simple.sc
```

When to use which
-----------------

- You changed AST, CFG, types, data flow, or a query: write a **unit test**.
- You changed the console, a launcher, packaging, script loading, or the query database distribution: use an
  **integration test**, and run the distribution test locally.
- You added a new frontend: add a unit test suite, and add a small project to [tests/code](../tests/code) and an
  entry to `frontend_configs` in [testDistro.py](../testDistro.py).
