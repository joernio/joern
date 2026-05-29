load("//bazel:scala.bzl", "scala_library")

scala_library(
  name = "semanticcpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [
    "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
    "@maven//:org_slf4j_slf4j_api",
    "@maven//:com_michaelpollmeier_scala_repl_pp_3_3_6",
    "@maven//:org_apache_commons_commons_text",
    "@maven//:org_apache_commons_commons_lang3",
    "@maven//:org_json4s_json4s_native_3",
    "@maven//:org_json4s_json4s_native_core_3",
    "@maven//:org_json4s_json4s_core_3",
    "@maven//:org_json4s_json4s_ast_3",
    "@maven//:org_scala_lang_modules_scala_xml_3",
    "@maven//:com_google_protobuf_protobuf_java",
  ],
  visibility = [ "//visibility:public" ],
)
