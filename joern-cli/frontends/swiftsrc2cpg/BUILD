load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

filegroup(
    name = "swiftsrc2cpg_astgen",
    srcs = select({
        "@bazel_tooling//platform:all": [
            "@swiftsrc2cpg_astgen_linux_arm//file",
            "@swiftsrc2cpg_astgen_linux_x86//file",
            "@swiftsrc2cpg_astgen_macos//file",
            "@swiftsrc2cpg_astgen_win_x86//file",
        ],
        "@bazel_tooling//platform:macos-arm": ["@swiftsrc2cpg_astgen_macos//file"],
        "@bazel_tooling//platform:macos-x86": ["@swiftsrc2cpg_astgen_macos//file"],
        "@bazel_tooling//platform:linux-arm": ["@swiftsrc2cpg_astgen_linux_arm//file"],
        "@bazel_tooling//platform:linux-x86": ["@swiftsrc2cpg_astgen_linux_x86//file"],
        "@bazel_tooling//platform:win-x86": ["@swiftsrc2cpg_astgen_win_x86//file"],
    }),
)

scala_library(
    name = "swiftsrc2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    data = [":swiftsrc2cpg_astgen"],
    resources = glob(["src/main/resources/application.conf"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_google_code_gson_gson",
        "@maven//:com_googlecode_plist_dd_plist",
        "@maven//:org_apache_commons_commons_lang3",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    # TODO Make hermetic
    # Use system path instead of the restrict PATH that Bazel usually uses when running.
    # That way external swift toolchain calls work.
    env_inherit = ["PATH"],
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    # We cannot sandbox the test because the swift compiler uses a sandbox itself
    # and sandboxes cannot be nested (at least no those initialized with sandbox-exec).
    tags = ["no-sandbox"],
    deps = [
        ":swiftsrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//semanticcpg",
    ],
)

java_binary(
    name = "swiftsrc2cpg-bin",
    main_class = "io.joern.swiftsrc2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":swiftsrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "swiftsrc2cpg-dist",
    srcs = [":swiftsrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
