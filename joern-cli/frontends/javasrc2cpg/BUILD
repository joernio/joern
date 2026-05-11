load("@rules_scala//scala:scala.bzl", "scala_library")

scala_library(
  name = "javasrc2cpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    "//:io_shiftleft_codepropertygraph_with_deps",
    "//joern-cli/frontends/x2cpg",
    "@joern_maven//:com_github_javaparser_javaparser_symbol_solver_core",
    "@joern_maven//:org_ow2_asm_asm",
    "@joern_maven//:org_slf4j_slf4j_api",
    "@joern_maven//:org_gradle_gradle_tooling_api",
    "@joern_maven//:org_projectlombok_lombok",
    "@joern_maven//:net_lingala_zip4j_zip4j",
    "@joern_maven//:org_scala_lang_modules_scala_parser_combinators_3",
    "@joern_maven//:org_scala_lang_modules_scala_parallel_collections_3",
    "@joern_maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)
