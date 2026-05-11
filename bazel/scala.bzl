load("@bazel_tooling//:scala_rule_factory.bzl", "make_scala_rules")

_rules = make_scala_rules(
    scala_version = "3.7.4",
    common_scalacopts = [
        "-java-output-version",
        "11",
        "-no-indent",
        "-old-syntax",
        "-Wshadow:type-parameter-shadow",
        "-deprecation",
        "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
    ],
    common_scalatest_runtime_deps = [
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

scala_library = _rules.scala_library
scala_binary = _rules.scala_binary
scala_test = _rules.scala_test
