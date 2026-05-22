load("//bazel:scala.bzl", "scala_library")

scala_library(
  name = "semanticcpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    "//:io_shiftleft_codepropertygraph_with_deps",
    "@joern_maven//:org_slf4j_slf4j_api",
    "@joern_maven//:com_michaelpollmeier_scala_repl_pp_3_3_6",
    "@joern_maven//:org_apache_commons_commons_text",
    "@joern_maven//:org_apache_commons_commons_lang3",
    "@joern_maven//:org_json4s_json4s_native_3",
    "@joern_maven//:org_json4s_json4s_native_core_3",
    "@joern_maven//:org_json4s_json4s_core_3",
    "@joern_maven//:org_json4s_json4s_ast_3",
    "@joern_maven//:org_scala_lang_modules_scala_xml_3",
    "@joern_maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)
