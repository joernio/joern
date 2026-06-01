load("//bazel:scala.bzl", "scala_library", "scala_test")
load("//bazel:java.bzl", "java_binary")
load("@rules_pkg//pkg:tar.bzl", "pkg_tar")

scala_library(
    name = "kotlin2cpg",
    srcs = glob(["src/main/scala/**/*.scala"]),
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        "//joern-cli/frontends/x2cpg",
        "//joern-cli/frontends/javasrc2cpg",
        "@maven//:com_lihaoyi_requests_3",
        "@maven//:com_lihaoyi_ujson_3",
        "@maven//:com_squareup_tools_build_maven_archeologist",
        "@maven//:org_gradle_gradle_tooling_api",
        "@maven//:org_jetbrains_kotlin_kotlin_stdlib_jdk8",
        "@maven//:org_jetbrains_kotlin_kotlin_stdlib",
        "@maven//:org_jetbrains_kotlin_kotlin_compiler_embeddable",
        "@maven//:org_jetbrains_kotlin_kotlin_allopen",
        "@maven//:com_google_protobuf_protobuf_java",
    ],
    resources = glob(["src/main/resources/jars/*"]),
    visibility = ["//visibility:public"],
)

scala_test(
    name = "tests",
    srcs = glob(["src/test/scala/**/*.scala"]),
    deps = [
        "//bazel/reexports:io_shiftleft_codepropertygraph_with_deps",
        ":kotlin2cpg",
        "//joern-cli/frontends/x2cpg:tests",
        "//dataflowengineoss",
        "//dataflowengineoss:tests",
    ],
    resources = glob(["src/main/resources/log4j2.xml"]),
    data = glob([
        "src/test/resources/code/ktmin/dependencies/*",
        "src/test/resources/jars/*",
        "src/test/resources/code/springboot-kotlin-webgoat/*",
    ]),
    env = {
      "FILE_IN_PROJECT_ROOT": "joern-cli/frontends/kotlin2cpg",
    },
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    jvm_flags = ["-ea"],
)

java_binary(
    name = "kotlin2cpg-bin",
    main_class = "io.joern.kotlin2cpg.Main",
    runtime_deps = [
        ":kotlin2cpg",
        "@maven//:org_apache_logging_log4j_log4j_slf4j2_impl",
    ],
    resources = glob(["src/main/resources/log4j2.xml"]),
    visibility = ["//visibility:public"],
)

pkg_tar(
    name = "kotlin2cpg-dist",
    srcs = [":kotlin2cpg-bin"],
    include_runfiles = True,
    visibility = ["//visibility:public"],
)
