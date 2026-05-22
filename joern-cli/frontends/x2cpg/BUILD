load("//bazel:scala.bzl", "scala_library", "scala_test")

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

scala_test(
  name = "tests",
  srcs = glob(["src/test/scala/**/*.scala"]),
  scalacopts = [ 
  ],  
  deps = [ 
    ":x2cpg",
    "//:io_shiftleft_codepropertygraph_with_deps",
    "@joern_maven//:org_apache_commons_commons_lang3",
    "@joern_maven//:com_google_protobuf_protobuf_java",
  ],
  # TODO tests should not depend on main resources
  data = glob(["src/test/resources/**/*", "src/main/resources/**/*"]),
  env = {
    "FILE_IN_PROJECT_ROOT": "joern-cli/frontends/x2cpg",
  },
  visibility = [ "//visibility:public" ],
)
