load("//bazel:java.bzl", "java_library")
load("//bazel:scala.bzl", "scala_library", "scala_test")

# We need to use the the java_library rule because the dependency
# @rules_java//java/runfiles brings in an annotation processor and
# those only get picked up and executed by java_library and not
# scala_library.
java_library(
    name = "x2cpg_java",
    srcs = glob(["src/main/javaBazel/**/*.java"]),
    deps = [
        "@rules_java//java/runfiles",
    ],
)

scala_library(
    name = "x2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    resources = [
        # TODO This should not be here but it is mimicking what the SBT
        # build does. This is just the most convienent position for this
        # file as resource because via x2cpg it ends up in the php2cpg
        # tests and in joern cli and it is need in both. Of course this
        # is a big no no since that way it gets distributed with each
        # and every frontend.
        "src/main/resources/php_known_function_signatures.txt",
    ],
    scalacopts = [
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":x2cpg_java",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//semanticcpg",
        "@maven//:com_lihaoyi_geny_3",
        "@maven//:com_lihaoyi_ujson_3",
        "@maven//:com_michaelpollmeier_versionsort",
        "@maven//:com_typesafe_config",
        "@maven//:org_apache_commons_commons_exec",
        "@maven//:org_gradle_gradle_tooling_api",
        "@maven//:org_apache_maven_maven_artifact",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    # TODO tests should not depend on main resources
    data = glob([
        "src/test/resources/**/*",
        "src/main/resources/**/*",
    ]),
    env = {
        "FILE_IN_PROJECT_ROOT": "joern-cli/frontends/x2cpg",
    },
    scalacopts = [
    ],
    deps = [
        ":x2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg/src/test/scala/io/joern/x2cpg/testfixtures",
        "@maven//:org_apache_commons_commons_lang3",
    ],
)
