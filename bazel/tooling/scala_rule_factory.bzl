load("@rules_scala//scala:scala.bzl", _scala_binary = "scala_binary", _scala_library = "scala_library", _scala_test = "scala_test")

def make_scala_rules(scala_version,
                     common_scalacopts,
                     common_scalatest_runtime_deps,
                     ):
    def scala_library(scalacopts = [], **kwargs):
        _scala_library(
          scalacopts = common_scalacopts + scalacopts,
          scala_version = scala_version,
          **kwargs)

    def scala_binary(scalacopts = [], **kwargs):
        _scala_binary(
          scalacopts = common_scalacopts + scalacopts,
          scala_version = scala_version,
          **kwargs)

    def scala_test(scalacopts = [], runtime_deps = [], **kwargs):
        _scala_test(
          scalacopts = common_scalacopts + scalacopts,
          scala_version = scala_version,
          runtime_deps = common_scalatest_runtime_deps + runtime_deps,
          **kwargs)

    return struct(
        scala_library = scala_library,
        scala_binary = scala_binary,
        scala_test = scala_test,
    )
