load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "c2cpg",
    srcs = glob([
        "src/main/scala/**/*.scala",
    ]),
    data = [
    ],
    resources = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_lihaoyi_ujson_3",
        "@maven//:io_joern_eclipse_cdt_core",
        "@maven//:org_apache_commons_commons_lang3",
        "@maven//:org_eclipse_platform_org_eclipse_core_resources",
        "@maven//:org_eclipse_platform_org_eclipse_core_runtime",
        "@maven//:org_eclipse_platform_org_eclipse_equinox_common",
        "@maven//:org_eclipse_platform_org_eclipse_osgi",
        "@maven//:org_eclipse_platform_org_eclipse_text",
        "@maven//:org_jline_jline",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = [
    ],
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":c2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//semanticcpg",
        "@maven//:com_github_scopt_scopt_3",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

java_binary(
    name = "c2cpg-bin",
    main_class = "io.joern.c2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":c2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "c2cpg-dist",
    srcs = [":c2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
