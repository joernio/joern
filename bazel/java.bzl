load("@bazel_tooling//:java_rule_factory.bzl", "make_java_rules")

_rules = make_java_rules(
    java_common_opts = [
        "-g",
        "-Xlint",
        "--release=11",
    ],
)

java_library = _rules.java_library
java_binary = _rules.java_binary
java_test = _rules.java_test
java_import = _rules.java_import
