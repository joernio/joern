load("//bazel:scala.bzl", "scala_library")

scala_library(
    name = "testfixtures",
    srcs = glob(["**/*.scala"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_google_protobuf_protobuf_java",
        "@rules_scala//testing/toolchain:scalatest_classpath",
    ],
)
