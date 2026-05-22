load("@rules_scala//scala:scala_toolchain.bzl", "scala_toolchain")
load(":check_version.bzl", "assert_scala_version")

scala_toolchain(
    name = "plus_one_toolchain_impl",
    dependency_mode = "plus-one",
    dependency_tracking_method = "ast",
)

# This check is here to fail early if the configured Scala version is out of sync with the 'plus_one_toolchain' target_settings.
# Although it is not really directly checking the version mismatch, it is hopefully close enough to
# the config setting so that people easily find it.
# The Scala version in target_settings must be kept in sync with the main Scala version because otherwise
# our custom toolchain will not be selected, the default toolchain will be choosen and will then show
# a ton of unrelatable errors where people cant wrap their head around how a Scala version bump can trigger
# this.
assert_scala_version("3.7.4", "Scala version in 'plus_one_toolchain' target_settings must be kept in sync with main Scala version.")

toolchain(
    name = "plus_one_toolchain",
    toolchain = ":plus_one_toolchain_impl",
    toolchain_type = "@rules_scala//scala:toolchain_type",
    target_settings = ["@@rules_scala++scala_config+rules_scala_config//:scala_version_3_7_4"],
)
