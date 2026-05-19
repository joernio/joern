load("@rules_scala//scala:scala.bzl", "scala_library")
load("@rules_java//java:defs.bzl", "java_binary")

java_binary(
  name = "java_cc",
  main_class = "org.javacc.parser.Main",
  runtime_deps = [
    "@joern_maven//:net_java_dev_javacc_javacc",
  ],
)

genrule(
  name = "generated_parser_code",
  srcs = ["pythonGrammar.jj"],
  outs = ["generated_classes.srcjar"],
  tools = [":java_cc"],
  toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
  cmd = """
  $(location :java_cc) -OUTPUT_DIRECTORY=$(RULEDIR)/generated_classes/io/joern/pythonparser $(SRCS)
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

