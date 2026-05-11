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
        "@maven//:com_google_protobuf_protobuf_java",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = [
    ],
    runtime_deps = [
    ],
    deps = [
        ":pysrc2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss:tests",
        "//joern-cli/frontends/x2cpg:tests",
        "//semanticcpg",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)
