load("//bazel:scala.bzl", "scala_library", "scala_test")
load("//bazel:java.bzl", "java_binary")

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
  tools = [":antlr_tool"],
  toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
  cmd = """
  $(location :antlr_tool) -package io.joern.dataflowengineoss $< -o $(RULEDIR)/generated_classes
  $(JAVABASE)/bin/jar --create --file $@ -C $(RULEDIR)/generated_classes .
  """,
)

scala_library(
  name = "dataflowengineoss",
  srcs = glob(["src/main/scala/**/*.scala"]) + [":generated_parser_code"],
  scalacopts = [ 
  ],  
  deps = [ 
    "//:io_shiftleft_codepropertygraph_with_deps",
    "//semanticcpg",
    "//joern-cli/frontends/x2cpg",
    "@maven//:org_antlr_antlr4_runtime",
    "//:com_lihaoyi_upickle_with_deps",
    "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
  ],
  visibility = [ "//visibility:public" ],
)

scala_test(
  name = "tests",
  srcs = glob(["src/test/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    ":dataflowengineoss",
    "//joern-cli/frontends/x2cpg:tests",
    "//:io_shiftleft_codepropertygraph_with_deps",
  ],
  visibility = [ "//visibility:public" ],
)
