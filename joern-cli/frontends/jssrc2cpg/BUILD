load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
  name = "jssrc2cpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  deps = [
    "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
    "//joern-cli/frontends/x2cpg",
    "//bazel/reexports:com_lihaoyi_upickle_with_deps",
    "@maven//:org_apache_commons_commons_lang3",
    "@maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)

scala_test(
  name = "tests",
  srcs = glob(["src/test/scala/**/*.scala"]),
  deps = [
    "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
    ":jssrc2cpg",
    "//joern-cli/frontends/x2cpg:tests",
    "//dataflowengineoss",
    "//dataflowengineoss:tests",
  ],
)
