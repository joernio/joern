load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")

config_setting(
    name = "macos-arm",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:arm64",
    ],
    flag_values = {":all_platforms_flag": "false"},
)

config_setting(
    name = "macos-x86",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:x86_64",
    ],
    flag_values = {":all_platforms_flag": "false"},
)

config_setting(
    name = "linux-arm",
    constraint_values = [
        "@platforms//os:linux",
        "@platforms//cpu:arm64",
    ],
    flag_values = {":all_platforms_flag": "false"},
)

config_setting(
    name = "linux-x86",
    constraint_values = [
        "@platforms//os:linux",
        "@platforms//cpu:x86_64",
    ],
    flag_values = {":all_platforms_flag": "false"},
)

config_setting(
    name = "win-x86",
    constraint_values = [
        "@platforms//os:windows",
        "@platforms//cpu:x86_64",
    ],
    flag_values = {":all_platforms_flag": "false"},
)

config_setting(
    name = "all",
    flag_values = {":all_platforms_flag": "true"},
)

bool_flag(
    name = "all_platforms_flag",
    build_setting_default = False,
    visibility = ["//visibility:public"],
)
