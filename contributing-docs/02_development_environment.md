Development environment
=======================

Joern can be developed on Linux, macOS and Windows. CI runs the tests on all three.

Requirements
------------

1. A JDK. CI uses Temurin 21, which is what we recommend. The build compiles with `--release 17`, so do not use
   APIs newer than Java 17.
2. [sbt](https://www.scala-sbt.org). The build uses sbt 2.x, the exact version is in
   [project/build.properties](../project/build.properties). The `sbt` launcher downloads it for you.
3. Network access on the first build. sbt downloads dependencies, and several frontends download AST generator
   binaries from GitHub releases.

sbt acts as a package manager, so after you installed a JDK and sbt, it downloads everything else.

```bash
git clone https://github.com/joernio/joern.git
cd joern
sbt c2cpg/compile
```

The JVM options for sbt are in [.sbtopts](../.sbtopts) (`-Xmx8G`). The build is memory hungry, so make sure
your machine has enough RAM.

External tools for tests
------------------------

Most tests only need the JDK. Some frontends invoke external tools. If a tool is missing, the corresponding tests
fail, so install the tools for the frontends you work on:

| Frontend | Needed for tests |
|----------|------------------|
| `c2cpg` | _optional_: `gcc` and `g++`, for auto-discovery of system header files |
| `php2cpg` | `php` on the `PATH` (`brew install php` on macOS) |
| `gosrc2cpg` | Go (CI uses 1.25) |
| `swiftsrc2cpg` | A Swift toolchain (CI uses 6.1, and 6.3 on macOS to match the Xcode SDK) |
| `rust2cpg` | A Rust toolchain with the `rust-src` component |
| `jssrc2cpg`, `csharpsrc2cpg`, `abap2cpg` | Nothing, the AST generator is downloaded |
| `rubysrc2cpg`, `kotlin2cpg`, `javasrc2cpg`, `jimple2cpg`, `pysrc2cpg` | Nothing, they run on the JVM |

The [PR workflow](../.github/workflows/pr.yml) is the source of truth for tool versions.

For the distribution test ([integration tests](05_integration_tests.md)) you also need Python 3 with `pexpect`
(`wexpect` on Windows).

Downloaded AST generators
-------------------------

Frontends that use an external AST generator download it as part of `compile` (sbt task `astGenDownloadTask`)
and stage it into the frontend's `bin/` directory. These files are git-ignored. Do not commit them.

- The version is set in the frontend's `src/main/resources/application.conf`, for example `astgen_version` in
  `jssrc2cpg`. For `php2cpg` it is `Versions.phpParser` in [project/Versions.scala](../project/Versions.scala).
- If a compatible `astgen` is already on your `PATH`, the download is skipped.
- Releases are published in [joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo).

IDE support
-----------

### IntelliJ IDEA

1. Install IntelliJ IDEA and the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala).
2. Open `sbt` in your local joern repository, run `compile` and **keep it open**. This allows the BSP build in the
   next step.
3. In IntelliJ, open the project and select your local clone. Open it as a **BSP project**, not as an `sbt project`.
4. Wait for the import and indexing to complete. Then you can build, or run a single test.

### VS Code

Use either a local setup with Metals, or the dev container in [.devcontainer](../.devcontainer):

1. Install VS Code, Docker and the `ms-vscode-remote.remote-containers` extension.
2. Open the Joern folder. Choose `Reopen in Container`, or run `Dev Containers: Reopen in Container`.
3. Run `Metals: Import build`.

Bazel
-----

There is an experimental, partial Bazel build. It is not used in CI or for releases. See [bazel.md](../bazel.md).
Use sbt unless you have a reason not to.

sbt on the command line
-----------------------

Starting sbt takes a while. For repeated commands use either an interactive `sbt` shell, or the thin client,
which talks to a running sbt server:

```bash
sbt --client --color=false --supershell=false --batch "c2cpg/testOnly *MethodTests"
```

`--color=false --supershell=false --batch` gives clean output, which is useful in scripts and for coding agents.
