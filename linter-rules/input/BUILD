load("//bazel:scala.bzl", "scala_library")

# Fixtures for the Scalafix testkit tests in //linter-rules:tests.
scala_library(
    name = "input",
    srcs = glob(["src/main/scala/**/*.scala"]),
    scalacopts = [
        "-feature",
    ],
    tags = ["no-scalafix"],  # contains violations on purpose
    visibility = ["//linter-rules:__pkg__"],
)

# The testkit reads the sources at test time (expected diagnostics are comments in them).
filegroup(
    name = "sources",
    srcs = glob(["src/main/scala/**/*.scala"]),
    visibility = ["//linter-rules:__pkg__"],
)
