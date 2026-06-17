load("//bazel:java.bzl", "java_library")

java_library(
    name = "nongenericwithsuperclass",
    srcs = glob(["*.java"]),
    javacopts = [
        "-Xep:ComparableType:OFF",
    ],
    visibility = ["//visibility:public"],
    deps = [
    ],
)
