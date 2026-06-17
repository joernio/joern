load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "rubysrc2cpg",
    srcs = glob([
        "src/main/scala/**/*.scala",
    ]),
    data = [
        "@rubysrc2cpg_astgen//:files",
        "@rubysrc2cpg_type_stubs//file",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_google_protobuf_protobuf_java",
        "@maven//:org_apache_commons_commons_compress",
        "@maven//:org_jruby_jruby_complete",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":rubysrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss:tests",
        "//joern-cli/frontends/x2cpg:tests",
        "//semanticcpg",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

java_binary(
    name = "rubysrc2cpg-bin",
    main_class = "io.joern.rubysrc2cpg.Main",
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":rubysrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "rubysrc2cpg-dist",
    srcs = [":rubysrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
