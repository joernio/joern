load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

filegroup(
    name = "rust2cpg_astgen",
    srcs = select({
        "@bazel_tooling//platform:all": [
            "@rust2cpg_astgen_linux_arm//file",
            "@rust2cpg_astgen_linux_x86//file",
            "@rust2cpg_astgen_macos_arm//file",
            "@rust2cpg_astgen_macos_x86//file",
            "@rust2cpg_astgen_win_arm//file",
            "@rust2cpg_astgen_win_x86//file",
        ],
        "@bazel_tooling//platform:macos-arm": ["@rust2cpg_astgen_macos_arm//file"],
        "@bazel_tooling//platform:macos-x86": ["@rust2cpg_astgen_macos_x86//file"],
        "@bazel_tooling//platform:linux-arm": ["@rust2cpg_astgen_linux_arm//file"],
        "@bazel_tooling//platform:linux-x86": ["@rust2cpg_astgen_linux_x86//file"],
        "@bazel_tooling//platform:win-arm": ["@rust2cpg_astgen_win_x86//file"],
        "@bazel_tooling//platform:win-x86": ["@rust2cpg_astgen_win_x86//file"],
    }),
)

scala_library(
    name = "rust2cpg",
    srcs = [
        "@rust2cpg_node_syntax//file:RustNodeSyntax.scala",
    ] + glob(["src/main/scala/**/*.scala"]),
    data = [":rust2cpg_astgen"],
    resources = glob(["src/main/resources/application.conf"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_lihaoyi_ujson_3",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    # TODO Make hermetic
    # Use system path instead of the restrict PATH that Bazel usually uses then running.
    # That way external rust toolchain calls work.
    env_inherit = ["PATH"],
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":rust2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//semanticcpg",
        "@maven//:com_lihaoyi_geny_3",
        "@maven//:com_lihaoyi_ujson_3",
    ],
)

java_binary(
    name = "rust2cpg-bin",
    main_class = "io.joern.rust2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":rust2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "rust2cpg-dist",
    srcs = [":rust2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
