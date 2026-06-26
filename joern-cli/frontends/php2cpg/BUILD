load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "php2cpg",
    srcs = glob([
        "src/main/scala/**/*.scala",
    ]),
    data = [
        "@php-parser//file",
    ],
    resources = [
        "src/main/resources/ClassParser.php",
        "src/main/resources/builtin_functions.txt",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_github_albfernandez_juniversalchardet",
        "@maven//:com_github_sh4869_semver_parser_scala_3",
        "@maven//:com_google_protobuf_protobuf_java",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = [
    ],
    # TODO Make hermetic
    # Use system path instead of the restrict PATH that Bazel usually uses then running.
    # That way external php can be used.
    env_inherit = ["PATH"],
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":php2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//joern-cli/frontends/x2cpg/src/test/scala/io/joern/x2cpg/testfixtures",
        "//semanticcpg",
        "@maven//:com_github_scopt_scopt_3",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

java_binary(
    name = "php2cpg-bin",
    main_class = "io.joern.php2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":php2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "php2cpg-dist",
    srcs = [":php2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
