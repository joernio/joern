load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

java_binary(
    name = "antlr_tool",
    main_class = "org.antlr.v4.Tool",
    runtime_deps = [
        "@maven//:org_antlr_antlr4",
    ],
)

genrule(
    name = "generated_parser_code",
    srcs = ["src/main/antlr4/io/joern/dataflowengineoss/Semantics.g4"],
    outs = ["generated_classes.srcjar"],
    cmd = """
  $(location :antlr_tool) -package io.joern.dataflowengineoss $< -o $(RULEDIR)/generated_classes
  $(JAVABASE)/bin/jar --create --file $@ -C $(RULEDIR)/generated_classes .
  """,
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    tools = [":antlr_tool"],
)

scala_library(
    name = "dataflowengineoss",
    srcs = glob(["src/main/scala/**/*.scala"]) + [":generated_parser_code"],
    scalacopts = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:com_lihaoyi_upickle_with_deps",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "//semanticcpg",
        "@maven//:org_antlr_antlr4_runtime",
        "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    scalacopts = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":dataflowengineoss",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg:tests",
    ],
)
