load("@aspect_rules_lint//format:defs.bzl", "format_multirun")
load("//bazel:java.bzl", "java_binary")

java_binary(
    name = "scalafmt",
    data = ["//:.scalafmt.conf"],
    main_class = "org.scalafmt.cli.Cli",
    runtime_deps = ["@format_maven//:org_scalameta_scalafmt_cli_2_13"],
)

format_multirun(
    name = "format",
    scala = ":scalafmt",
    visibility = ["//visibility:public"],
)
