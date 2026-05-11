load("@rules_scala_config//:config.bzl", "SCALA_VERSION")

def assert_scala_version(expected, error_message):
    if SCALA_VERSION != expected:
        fail(error_message)
