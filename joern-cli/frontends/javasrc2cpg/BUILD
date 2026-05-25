load("//bazel:scala.bzl", "scala_library")

scala_library(
  name = "javasrc2cpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    "//:io_shiftleft_codepropertygraph_with_deps",
    "//joern-cli/frontends/x2cpg",
    "@maven//:com_github_javaparser_javaparser_symbol_solver_core",
    "@maven//:org_ow2_asm_asm",
    "@maven//:org_slf4j_slf4j_api",
    "@maven//:org_gradle_gradle_tooling_api",
    "@maven//:org_projectlombok_lombok",
    "@maven//:net_lingala_zip4j_zip4j",
    "@maven//:org_scala_lang_modules_scala_parser_combinators_3",
    "@maven//:org_scala_lang_modules_scala_parallel_collections_3",
    "@maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)
