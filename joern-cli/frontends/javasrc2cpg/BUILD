load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "javasrc2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_github_javaparser_javaparser_symbol_solver_core",
        "@maven//:com_google_protobuf_protobuf_java",
        "@maven//:net_lingala_zip4j_zip4j",
        "@maven//:org_gradle_gradle_tooling_api",
        "@maven//:org_ow2_asm_asm",
        "@maven//:org_projectlombok_lombok",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
        "@maven//:org_scala_lang_modules_scala_parser_combinators_3",
        "@maven//:org_slf4j_slf4j_api",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = glob(["src/test/resources/**/*"]) + [
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/genericclasses",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/genericmethods",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/nongenericinterface",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/nongenericwithsuperclass",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/overloadedmethods",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/simplegeneric",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/simplenongeneric",
        "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/wildcards",
    ],
    env = {
        "JAVA_HOME": "$(JAVABASE_ROOTPATH)",
    },
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    deps = [
        ":javasrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss",
        "//dataflowengineoss:tests",
        "//joern-cli/frontends/x2cpg:tests",
        "//semanticcpg",
        "@maven//:com_github_javaparser_javaparser_symbol_solver_core",
        "@maven//:com_github_scopt_scopt_3",
    ],
)

java_binary(
    name = "javasrc2cpg-bin",
    main_class = "io.joern.javasrc2cpg.Main",
    resources = glob(["src/main/resources/log4j2.xml"]),
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":javasrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "javasrc2cpg-dist",
    srcs = [":javasrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
