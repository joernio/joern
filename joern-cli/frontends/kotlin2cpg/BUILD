load("@rules_pkg//pkg:tar.bzl", "pkg_tar")
load("//bazel:java.bzl", "java_binary")
load("//bazel:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "kotlin2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    resources = glob(["src/main/resources/jars/*"]),
    visibility = ["//visibility:public"],
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/javasrc2cpg",
        "//joern-cli/frontends/x2cpg",
        "@maven//:com_google_protobuf_protobuf_java",
        "@maven//:com_lihaoyi_requests_3",
        "@maven//:com_lihaoyi_ujson_3",
        "@maven//:com_squareup_tools_build_maven_archeologist",
        "@maven//:org_gradle_gradle_tooling_api",
        "@maven//:org_jetbrains_kotlin_kotlin_allopen",
        "@maven//:org_jetbrains_kotlin_kotlin_compiler_embeddable",
        "@maven//:org_jetbrains_kotlin_kotlin_stdlib",
        "@maven//:org_jetbrains_kotlin_kotlin_stdlib_jdk8",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    data = glob([
        "src/test/resources/code/ktmin/dependencies/*",
        "src/test/resources/jars/*",
        "src/test/resources/code/springboot-kotlin-webgoat/*",
    ]),
    env = {
        "FILE_IN_PROJECT_ROOT": "joern-cli/frontends/kotlin2cpg",
    },
    jvm_flags = ["-ea"],
    resources = [
        "src/main/resources/log4j2.xml",
    ],
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    deps = [
        ":kotlin2cpg",
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//dataflowengineoss",
        "//dataflowengineoss:tests",
        "//joern-cli/frontends/x2cpg:tests",
    ],
)

java_binary(
    name = "kotlin2cpg-bin",
    main_class = "io.joern.kotlin2cpg.Main",
    resources = ["src/main/resources/log4j2.xml"],
    visibility = ["//visibility:public"],
    runtime_deps = [
        ":kotlin2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
)

pkg_tar(
    name = "kotlin2cpg-dist",
    srcs = [":kotlin2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
