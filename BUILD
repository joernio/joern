load("@bazel_tooling//scalafix:defs.bzl", "scalafix")

exports_files([".scalafmt.conf"])

alias(
    name = "format",
    actual = "//bazel/format",
)

alias(
    name = "formatCheck",
    actual = "//bazel/format:format.check",
)

alias(
    name = "all_platforms",
    actual = "@bazel_tooling//platform:all_platforms_flag",
)

alias(
    name = "javasrc2cpg",
    actual = "//joern-cli/frontends/javasrc2cpg:javasrc2cpg-bin",
)

alias(
    name = "jssrc2cpg",
    actual = "//joern-cli/frontends/jssrc2cpg:jssrc2cpg-bin",
)

alias(
    name = "kotlin2cpg",
    actual = "//joern-cli/frontends/kotlin2cpg:kotlin2cpg-bin",
)

alias(
    name = "pysrc2cpg",
    actual = "//joern-cli/frontends/pysrc2cpg:pysrc2cpg-bin",
)

alias(
    name = "rubysrc2cpg",
    actual = "//joern-cli/frontends/rubysrc2cpg:rubysrc2cpg-bin",
)

alias(
    name = "rust2cpg",
    actual = "//joern-cli/frontends/rust2cpg:rust2cpg-bin",
)

alias(
    name = "swiftsrc2cpg",
    actual = "//joern-cli/frontends/swiftsrc2cpg:swiftsrc2cpg-bin",
)

# Bazel equivalent of:
#   scalafix --diff-base origin/master RestrictedImports SingleLetterIdentifiers UnorderedIteration
# Run with `bazel run //:scalafix` (fix) or `bazel run //:scalafix.check` (verify only).
scalafix(
    name = "scalafix",
    rules = [
        "RestrictedImports",
        "SingleLetterIdentifiers",
        "UnorderedIteration",
    ],
    rules_lib = "//linter-rules",
)
