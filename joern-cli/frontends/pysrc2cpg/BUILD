load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

java_binary(
    name = "java_cc_tool",
    main_class = "org.javacc.parser.Main",
    runtime_deps = [
        "@maven//:net_java_dev_javacc_javacc",
    ],
)

genrule(
    name = "generated_parser_code",
    srcs = ["pythonGrammar.jj"],
    outs = ["generated_classes.srcjar"],
    cmd = """
  $(location :java_cc_tool) -OUTPUT_DIRECTORY=$(RULEDIR)/generated_classes/io/joern/pythonparser $(SRCS)
  $(JAVABASE)/bin/jar --create --file $@ -C $(RULEDIR)/generated_classes .
  """,
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    tools = [":java_cc_tool"],
)

scala_library(
    name = "pysrc2cpg",
    srcs = glob([
        "src/main/scala/**/*.scala",
        "src/main/java/**/*.java",
    ]) + [
        ":generated_parser_code",
    ],
    scalacopts = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    deps = [
        ":pysrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss/src/test/scala/io/joern/dataflowengineoss/testfixtures",
        "//semanticcpg",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

java_binary(
    name = "pysrc2cpg-bin",
    main_class = "io.joern.pysrc2cpg.NewMain",
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":pysrc2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "pysrc2cpg-dist",
    srcs = [":pysrc2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
