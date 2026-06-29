load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

filegroup(
    name = "jssrc2cpg_astgen",
    srcs = select({
        "@bazel_tooling//platform:all": [
            "@jssrc2cpg_astgen_linux_arm//file",
            "@jssrc2cpg_astgen_linux_x86//file",
            "@jssrc2cpg_astgen_macos_arm//file",
            "@jssrc2cpg_astgen_macos_x86//file",
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
    data = [
        ":jssrc2cpg_astgen",
    ],
    resources = glob(["src/main/resources/application.conf"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:org_apache_commons_commons_lang3",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":jssrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//semanticcpg",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

java_binary(
    name = "jssrc2cpg-bin",
    main_class = "io.joern.jssrc2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":jssrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "jssrc2cpg-dist",
    srcs = [":jssrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
