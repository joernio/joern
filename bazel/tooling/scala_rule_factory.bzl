load("@rules_scala//scala:scala.bzl", _scala_binary = "scala_binary", _scala_library = "scala_library", _scala_test = "scala_test")

def make_scala_rules(scala_version,
                     scala_common_opts,
                     ):
    def scala_library(scalacopts = [], **kwargs):
        _scala_library(
          scalacopts = scala_common_opts + scalacopts,
          scala_version = scala_version,
          **kwargs)

    def scala_binary(scalacopts = [], **kwargs):
        _scala_binary(
          scalacopts = scala_common_opts + scalacopts,
          scala_version = scala_version,
          **kwargs)

    def scala_test(scalacopts = [], **kwargs):
        _scala_test(
          scalacopts = scala_common_opts + scalacopts,
          scala_version = scala_version,
          **kwargs)

    return struct(
        scala_library = scala_library,
        scala_binary = scala_binary,
        scala_test = scala_test,
    )
