load("@bazel_tooling//scalafix:defs.bzl", "scalafix_testkit_properties")
load("//bazel:scala.bzl", "scala_library", "scala_test")

# Custom Scalafix rules. Used via the scalafix runner in the root BUILD file (bazel run //:scalafix).
scala_library(
    name = "linter-rules",
    srcs = glob(["src/main/scala/**/*.scala"]),
    resources = glob(["src/main/resources/**"]),
    resource_strip_prefix = "linter-rules/src/main/resources",
    visibility = ["//visibility:public"],
    deps = [
        "@bazel_tooling//scalafix:scalafix_core",
    ],
)

scalafix_testkit_properties(
    name = "testkit_properties",
    input = "//linter-rules/input",
    input_source_dirs = ["linter-rules/input/src/main/scala"],
    scalac_options = [
        "-deprecation",
        "-feature",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = [
        "//linter-rules/input",
        "//linter-rules/input:sources",
    ],
    resource_strip_prefix = "linter-rules/testkit_properties",
    resources = [":testkit_properties"],
    deps = [
        ":linter-rules",
        "@bazel_tooling//scalafix:scalafix_testkit",
    ],
)
