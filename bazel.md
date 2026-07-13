# Important information about Bazel
The Bazel build is currently experimental and intended for developement purposes.
It is not used in CI or for artifact releases.
Currenlty only a subset of the entire repository can be build with Bazel.
E.g. only a subset of the frontends are included. Check for the
existence joern-cli/frontends/<frontend>/BUILD file to check if a frontend is already
included.

## Prerequisits
- Bazelisk - This is a frontend to Bazel which makes sure you have the right version
  of Bazel on your system. Use it since Bazel wont work if you dont use the exact
  version configured in `.bazelversion`.
- Intellij Idea > 6.1.0
- Bazel plugin for Intellij >2026.1.2 (Do not install the old Bazel plugin which is
  named (Bazel for Intellij(legacy))

## Getting started
- Configure local Bazel cache location and size in ~/.bazelrc
  ```
    common --disk_cache ~/.cache/bazelBuildCache
    common --experimental_disk_cache_gc_max_size=30G
  ```
- Consider cloning the entire repo a second time for working
  with Bazel, Intellij can get unhappy with switching between sbt and Bazel with
  old `.idea`, `.bsp` folders lying around.
- Load project into Intellij via `File->Open` otherwise you wont see the option to
  import as Bazel project.
- To run formatting via scalafmt use `bazel run format`. For format checking use
  `bazel run formatCheck`.
- To build all code belonging to the root project, use `bazel build //...`.
- To run all tests belonging to the root project, use `bazel test //...`.
- To build and run a frontend in one step, use `bazel run <frontendName> -- <args>`.
  E.g. `bazel run javasrc2cpg -- /tmp/someJavaCodebase -o /tmp/myJava.cpg`.
  This is so fast that there is no need for a separate starter script for a
  staged version in the development cycle.
- In Intellij Idea you find a Bazel navigation bar on the right side which lets
  you build/run/test all the targets in the root project.
- Check trouble shooting section

## Bazel configuration
Bazel looks for config files in a multitude of places. Most important for us is the
`.bazelrc` file which contains the shared and checked in project specific configuration.
Additionally Bazel looks at `~/.bazelrc` for user specific configuration. As per our
`.bazelrc` it also loads `user.bazelrc` and that is where the user specific and project
specific configuration goes. The config file lookup order is as follow where the later
overrides the former: `.bazelrc`, `user.bazelrc`, `~/.bazelrc`.

The bazelrc files start with a selector followed by the configuration flag.
The selector specifies to which bazel command the flag is applied to. The most common are:
- `build` applies to bazel build
- `test` applies to bazel test
- `run` applies to bazel run
- `common` applies to all bazel commands

The following flags are the the same as you would otherwise pass on the a single CLI invocation.

### Important configuration options
Examples for often used configuration options that go into one of the bazelrc files.
- Configure environment variable for tests: `<selector> --test_env=...`

  Important note for Intellij users: Running a test fixture is a `bazel test` command,
  so `test` selector must be used. Debugging a test fixture is a `bazel run` command,
  so `run` selector must be used. To avoid duplication `common` can be used as selector.

- Configure JVM options for tests: `<selector> --test_env=JAVA_TOOL_OPTIONS=...`

## Update maven dependencies
To add or update maven dependencies the MODULE.bazel file needs to be modified.
Additionally the command `REPIN=1 bazel run @maven//:pin` needs to be run in the
repository root directory to regenerate the `maven_install.json` lock file.

## Multi-platform build
Some of our targets are platform dependend. This is most often the case if a frontend brings
in platform dependend AST generator components. By default the targets are build for the platform
which executes the build. A platform independend target build is created with the command line
flag `--//:all_platforms=true`. This packs binaries for all platforms into the resulting artifact.

## Trouble shooting
### Unresolved/Red symbols in Intellij
After loading the project for the first time in Intellij with `Open ...' there might be lots of
unresolved/red symbols especially around our semantic cpg language DSL. To fix this you need
to uncomment `import_ijars: true` in `.bazelbsp/.bazelproject` and resync the project. If
the mentioned line is already uncommented, comment it out and resync. It seems more important
that its status changes then the actual comment in/out status.
What can also help is cache invalidation: Go to `File -> Invalidate Caches...` and select
`Clear file system cache and Local History` as well as `Invalidate downloaded shared indexes`.

### Test caching and cache_test_results setting
By default, Bazel caches test results and replays them on subsequent runs without re-executing
the tests if the inputs haven't changed. Bazel indicates cached results with a small `(cached)`
notation in the output. While this is useful for performance, it can be problematic during
development when you want to re-run tests even though nothing has changed.

To disable test result caching, add the following to your `~/.bazelrc`:
```
test --cache_test_results=no
```

This setting affects both the CLI (`bazel test` commands) and IntelliJ, ensuring tests are
always re-executed regardless of whether the inputs have changed. This is particularly useful
during interactive development and debugging when you need to run tests frequently.

## Bazel build development notes

## Scalac options
Scalac options must not be provided in the double minus form (--) but instead only
with a single minus (-) or otherwise the compiler parameter parsing breaks.

### Scala and Java rules
We do not directly use scala_library, scala_binary and scala_test from rules_scala
because we want to be able to provide a default set of scalac options. Instead we
use our own wrappers from <joern>/bazel/tooling.
The same applies to the java_*** rules.

### Transitive data attribute passing
In the currently latest rules_scala version 7.2.4 there is a bug regarding the transitive
propagating of data attributes for runtime dependencies. Whenever a scala_*** rule references
another target via its `runtime_deps` attribute, the data attributes of the references and
their transitives are ignored.
There are two workarounds:
1. Use the `deps` attribute to reference those targets. This has the downside of making the
   dependencies symbols available to link against while that was not the intention and it
   makes the build slower since a change in the dependency triggers a recompilation.
2. Use java_*** rule to bring in the dependency. Problem is that this does not compound well
   since for every target that wants to just pass through such a dependency and has Scala
   code, we then need two targets one to handle the Scala code and the java rule which combines
   the Scala target with the runtime dependency.

In practice this is not much of a problem since runtime dependency are usually only brought
in at leaf targets. Those leafs are usualy binary target which only reference a main class and
libraries. So we do not even have Scala code and can directly use `java_binary`.

### Current problems (aka todo-list)
  1. Running and debugging tests in Intellij: There is some inconsistency, where some buttons
     try to use sbt (which fails if you're set up as a bazel projects) and others work.
  2. Intellij sometimes gets unhappy with type inference, where everything is painted red.
     We don't know reliable triggers for this bug, nor do we know a reliable fix, nor do
     we understand the reason for this.
  3. An important performance consideration: While debugging a specific test, the "modify 
     this single source file, run this specific test again" must be interactive-fast, not
     coffee-fast. Try this out!
     We tested it to be fast enough. But we would not be surprised if this depends on the
     specific buttons we pressed in Intellij. So please report if you find this slow.
  4. Scalafix integration is still TBD
