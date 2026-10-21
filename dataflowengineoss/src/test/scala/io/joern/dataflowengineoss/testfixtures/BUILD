load("//bazel:scala.bzl", "scala_library")

scala_library(
    name = "testfixtures",
    srcs = glob(["**/*.scala"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss",
        "//joern-cli/frontends/x2cpg/src/test/scala/io/joern/x2cpg/testfixtures",
    ],
)
