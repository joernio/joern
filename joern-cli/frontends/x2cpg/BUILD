load("@rules_scala//scala:scala.bzl", "scala_library")

scala_library(
  name = "x2cpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    "//semanticcpg",
    "//:io_shiftleft_codepropertygraph_with_deps",
    "@joern_maven//:com_lihaoyi_ujson_3",
    "@joern_maven//:com_lihaoyi_geny_3",
    "@joern_maven//:com_typesafe_config",
    "@joern_maven//:com_michaelpollmeier_versionsort",
    "@joern_maven//:org_gradle_gradle_tooling_api",
    "@joern_maven//:org_apache_commons_commons_exec",
  ],
  visibility = [ "//visibility:public" ],
)
