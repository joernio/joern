load("@bazel_tooling//:scala_rule_factory.bzl", "make_scala_rules")

_rules = make_scala_rules(
    scala_version = "3.8.3",
    common_scalacopts = [
        "-java-output-version",
        "17",
        "-no-indent",
        "-old-syntax",
        "-Wshadow:type-parameter-shadow",
        "-deprecation",
        "-language:implicitConversions",
        "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
    ],
    common_scala_binary_runtime_deps = [
        # We bring in the linked version of codepropertygraph domain classes as default/common
        # dependency so that people do not forget to add it. Missing it results in all kinds
        # of very hard to associate error messages.
        # Bringing it in explicitly for executables is necessary because codepropertygraph
        # target @codepropertygraph//schema:domainClassesNeverLinked has the attribute
        # `neverlink = True` which is necessary so that downstream projects can choose
        # their own domain class implementation like we do here.
        "@codepropertygraph//schema:domainClassesLinked",
    ],
    common_scala_test_runtime_deps = [
        "@codepropertygraph//schema:domainClassesLinked",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

scala_library = _rules.scala_library
scala_binary = _rules.scala_binary
scala_test = _rules.scala_test
