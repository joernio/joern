load("//bazel:scala.bzl", "scala_library", "scala_test")
load("//bazel:java.bzl", "java_binary")
load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:filter_runfiles.bzl", "filter_runfiles")

filegroup(
    name = "swiftsrc2cpg_astgen",
    srcs = select({
        "//bazel/platform:all": [
            "@swiftsrc2cpg_astgen_macos//file",
            "@swiftsrc2cpg_astgen_linux_x86//file",
            "@swiftsrc2cpg_astgen_linux_arm//file",
            "@swiftsrc2cpg_astgen_win_x86//file",
        ],
        "//bazel/platform:macos-arm": ["@swiftsrc2cpg_astgen_macos//file"],
        "//bazel/platform:macos-x86": ["@swiftsrc2cpg_astgen_macos//file"],
        "//bazel/platform:linux-arm": ["@swiftsrc2cpg_astgen_linux_arm//file"],
        "//bazel/platform:linux-x86": ["@swiftsrc2cpg_astgen_linux_x86//file"],
        "//bazel/platform:win-x86": ["@swiftsrc2cpg_astgen_win_x86//file"],
    }),
)

scala_library(
    name = "swiftsrc2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "@maven//:com_google_code_gson_gson",
        "@maven//:com_googlecode_plist_dd_plist",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
        "@maven//:org_apache_commons_commons_lang3",
        "@maven//:com_google_protobuf_protobuf_java",
    ],
    resources = glob(["src/main/resources/application.conf"]),
    data = [":swiftsrc2cpg_astgen"],
    visibility = ["//visibility:public"],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        ":swiftsrc2cpg",
        "//semanticcpg",
        "//dataflowengineoss:tests",
    ],
    resources = glob(["src/main/resources/log4j2.xml"]),
)

java_binary(
    name = "swiftsrc2cpg-bin",
    main_class = "io.joern.swiftsrc2cpg.Main",
    runtime_deps = [
        ":swiftsrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
    resources = glob(["src/main/resources/log4j2.xml"]),
    visibility = ["//visibility:public"],
)

pkg_tar(
    name = "swiftsrc2cpg-dist",
    srcs = [":swiftsrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
