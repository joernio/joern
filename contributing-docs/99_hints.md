Random tidbits
==============

### Build

- The first build is slow: sbt downloads dependencies, and frontends download their AST generators.
  Later builds are incremental. Prefer `sbt {module}/compile` over `sbt compile`.
- The build uses `-Werror`. If a new Scala or library version adds a warning, the build fails. Fix the cause.
- `Test / fork := true`: tests run in a forked JVM, so `System.getProperty`/environment changes in the sbt shell do
  not reach the tests. `integrationTests` is the exception, it does not fork.
- sbt 2.x sets `exportJars := true` by default. Joern turns it off (`exportJars := false`), because tests load
  resources as files and script compilation needs class directories. If you see a
  `FileSystemNotFoundException` in a test, that is the usual suspect.
- sbt 2.x caches tasks. If a task does not re-run although its inputs changed on disk, check that the input is a
  declared task input, or use `Def.uncached`. See existing tasks in the frontend `build.sbt` files for examples.
- Memory: [.sbtopts](../.sbtopts) sets `-Xmx8G`. If sbt is killed, check that your machine has the RAM.
- `sbt reload` is triggered automatically when build sources or a frontend `application.conf` change.

### Tests

- `sbt test` skips tests that already passed on unchanged inputs. Use `testFull` for a real full run.
- A test that fails only on one OS: check path separators, `\r\n` line endings, and file name case sensitivity.
- Tests that need the network are tagged `NetworkTest`.

### CPG

- The CPG schema comes from `codepropertygraph`. If a node type or property you need is missing, it has to be added
  there first.
- `code` is not a unique identifier. Use `fullName` where it is available.
- Output must be deterministic: no dependence on hash order, thread timing or the machine. The `UnorderedIteration`
  rule catches many but not all cases.
- Traversals are lazy `Iterator`s. A traversal that was consumed once is empty the second time. Use `.l` to
  materialize it if you need it more than once.

### Scalafix

- `scalafix --diff-base origin/master` needs `origin/master` to exist locally. Run `git fetch origin master` first.
- Scalafix needs the semanticdb output of the compile, so run it on code that compiles.
- To test or debug a rule, see `linter-rules/src/test` and the fixtures in `linter-rules/input`.

### Misc

- Shared guidance for coding agents belongs in [AGENTS.md](../AGENTS.md), and `CLAUDE.md` is a symlink to it.
  Personal notes and per-module hints can go into `CLAUDE.local.md` files and `.claude/settings.local.json`,
  which are git-ignored. Do not put anything in there that other contributors need.
- `bazel-*` directories in the repository root are Bazel output symlinks, they are git-ignored.
