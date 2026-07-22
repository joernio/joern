load("@bazel_tooling//:java_rule_factory.bzl", "make_java_rules")

_rules = make_java_rules(
    java_common_opts = [
        "-g",
        "-Xlint",
        "--release=17",
    ],
    common_java_binary_runtime_deps = [
        # We bring in the linked version of codepropertygraph domain classes as default/common
        # dependency so that people do not forget to add it. Missing it results in all kinds
        # of very hard to associate error messages.
        # Bringing it in explicitly for executables is necessary because codepropertygraph
        # target @codepropertygraph//schema:domainClassesNeverLinked has the attribute
        # `neverlink = True` which is necessary so that downstream projects can choose
        # their own domain class implementation like we do here.
        "@codepropertygraph//schema:domainClassesLinked",
    ],
    common_java_test_runtime_deps = [
        "@codepropertygraph//schema:domainClassesLinked",
    ],
)

java_library = _rules.java_library
java_binary = _rules.java_binary
java_test = _rules.java_test
java_import = _rules.java_import
