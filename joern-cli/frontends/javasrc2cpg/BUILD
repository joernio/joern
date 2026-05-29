load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
  name = "javasrc2cpg",
  srcs = glob(["src/main/scala/**/*.scala"]),
  deps = [
    "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
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

scala_test(
  name = "tests",
  srcs = glob(["src/test/scala/**/*.scala"]),
  deps = [
    "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
    ":javasrc2cpg",
    "//semanticcpg",
    "//joern-cli/frontends/x2cpg:tests",
    "//dataflowengineoss",
    "//dataflowengineoss:tests",
    "@maven//:com_github_javaparser_javaparser_symbol_solver_core",
    "@maven//:com_github_scopt_scopt_3",
  ],
  data = glob(["src/test/resources/**/*"]) + [
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/genericclasses",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/genericmethods",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/nongenericinterface",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/nongenericwithsuperclass",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/overloadedmethods",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/simplenongeneric",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/simplegeneric",
    "//joern-cli/frontends/javasrc2cpg/src/test/java/io/joern/javasrc2cpg/jartypereader/testcode/wildcards",
  ],
  env = {
    "FILE_IN_PROJECT_ROOT": "joern-cli/frontends/javasrc2cpg",
    "JAVA_HOME": "$(JAVABASE_ROOTPATH)",
  },
  toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
)
