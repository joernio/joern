load("//bazel:scala.bzl", "scala_library", "scala_test")
load("//bazel:java.bzl", "java_binary")

java_binary(
  name = "java_cc_tool",
  main_class = "org.javacc.parser.Main",
  runtime_deps = [
    "@joern_maven//:net_java_dev_javacc_javacc",
  ],
)

genrule(
  name = "generated_parser_code",
  srcs = ["pythonGrammar.jj"],
  outs = ["generated_classes.srcjar"],
  tools = [":java_cc_tool"],
  toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
  cmd = """
  $(location :java_cc_tool) -OUTPUT_DIRECTORY=$(RULEDIR)/generated_classes/io/joern/pythonparser $(SRCS)
  $(JAVABASE)/bin/jar --create --file $@ -C $(RULEDIR)/generated_classes .
  """,
)

scala_library(
  name = "pysrc2cpg",
  srcs = glob(["src/main/scala/**/*.scala", "src/main/java/**/*.java"]) + [
    ":generated_parser_code",
  ],
  scalacopts = [ 
  ],  
  deps = [ 
    "//:io_shiftleft_codepropertygraph_with_deps",
    "//joern-cli/frontends/x2cpg",
    "@joern_maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)

scala_test(
  name = "tests",
  srcs = glob(["src/test/scala/**/*.scala"]),
  deps = [
    ":pysrc2cpg",
    "//:io_shiftleft_codepropertygraph_with_deps",
    "//semanticcpg",
    "//dataflowengineoss:tests",
    "//joern-cli/frontends/x2cpg:tests",
    "@joern_maven//:org_scala_lang_modules_scala_parallel_collections_3",
  ],
  runtime_deps = [
  ],
  data = [
  ],
)
