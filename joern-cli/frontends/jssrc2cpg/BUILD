load("//bazel:scala.bzl", "scala_library", "scala_test")
load("//bazel:java.bzl", "java_binary")
load("@rules_pkg//pkg:tar.bzl", "pkg_tar")

filegroup(
    name = "jssrc2cpg_astgen",
    srcs = select({
        "@bazel_tooling//platform:all": [
            "@jssrc2cpg_astgen_macos_arm//file",
            "@jssrc2cpg_astgen_macos_x86//file",
            "@jssrc2cpg_astgen_linux_arm//file",
            "@jssrc2cpg_astgen_linux_x86//file",
            "@jssrc2cpg_astgen_win_x86//file",
            ],
        "@bazel_tooling//platform:macos-arm": ["@jssrc2cpg_astgen_macos_arm//file"],
        "@bazel_tooling//platform:macos-x86": ["@jssrc2cpg_astgen_macos_x86//file"],
        "@bazel_tooling//platform:linux-arm": ["@jssrc2cpg_astgen_linux_arm//file"],
        "@bazel_tooling//platform:linux-x86": ["@jssrc2cpg_astgen_linux_x86//file"],
        "@bazel_tooling//platform:win-x86": ["@jssrc2cpg_astgen_win_x86//file"],
      }),
)

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
  resources = glob(["src/main/resources/application.conf"]),
  data = [
      ":jssrc2cpg_astgen"
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
  resources = glob(["src/main/resources/log4j2.xml"]),
)

java_binary(
  name = "jssrc2cpg-bin",
  main_class = "io.joern.jssrc2cpg.Main",
  runtime_deps = [
    ":jssrc2cpg",
    "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
  ],
  resources = glob(["src/main/resources/log4j2.xml"]),
  visibility = ["//visibility:public"],
)

pkg_tar(
  name = "jssrc2cpg-dist",
  srcs = [":jssrc2cpg-bin"],
  include_runfiles = True,
  visibility = ["//visibility:public"],
)
