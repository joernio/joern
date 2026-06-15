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
- In Intellij Idea you find a Bazel navigation bar on the right side which lets
  you build/run/test all the targets in the root project.

## Update maven dependencies
To add or update maven dependencies the MODULE.bazel file needs to be modified.
Additionally the command `REPIN=1 bazel run @maven//:pin` needs to be run in the
repository root directory to regenerate the `maven_install.json` lock file.

## Release Joern
Clone `https://git0.harness.io/l7B_kbSEQD2wjrM7PShm5w/PROD/Qwiet_AI/bazel-registry.git`.
Run:
- `release-module.sh --module <joernRepo>/MODULE.bazel --version <version> --commit <commit>`.
- `release-module.sh --module <joernRepo>/bazel/tooling/MODULE.bazel --version <version> --commit <commit>`.

The script modifies the `modules` directory in your checkout of `bazel-registry`. Commit those changes and
create a PR. Once the are on master, they are fully released and can be consumed by the CS build.

## Scalac options
Scalac options must not be provided in the double minus form (--) but instead only
with a single minus (-) or otherwise the compiler parameter parsing breaks.

## Bazel build development notes

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
  4. If you re-run tests without inputs having changed, Bazel sometimes doesn't re-run tests
     and instead simply replays the old results. Bazel informs you about this by a small 
     `(cached)` indication. This is cool if everything works but check if that is really
     what you want.
  5. Scalafix integration is still TBD
