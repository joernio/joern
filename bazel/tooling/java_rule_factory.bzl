load(
    "@rules_java//java:defs.bzl",
    _java_binary = "java_binary",
    _java_import = "java_import",
    _java_library = "java_library",
    _java_test = "java_test",
)

def make_java_rules(java_common_opts,
        common_java_binary_runtime_deps,
        common_java_test_runtime_deps):
    def java_binary(javacopts = [], runtime_deps = [], ignore_common_runtime_deps = False, **kwargs):
        runtime_deps_combined = []
        if (not ignore_common_runtime_deps):
            runtime_deps_combined += common_java_binary_runtime_deps
        runtime_deps_combined += runtime_deps

        _java_binary(
            javacopts = java_common_opts + javacopts,
            runtime_deps = runtime_deps_combined,
            **kwargs
        )

    def java_library(javacopts = [], **kwargs):
        _java_library(
            javacopts = java_common_opts + javacopts,
            **kwargs
        )

    def java_test(javacopts = [], runtime_deps = [], ignore_common_runtime_deps = False, **kwargs):
        runtime_deps_combined = []
        if (not ignore_common_runtime_deps):
            runtime_deps_combined += common_java_test_runtime_deps
        runtime_deps_combined += runtime_deps

        _java_test(
            javacopts = java_common_opts + javacopts,
            runtime_deps = runtime_deps_combined,
            **kwargs
        )

    def java_import(**kwargs):
        _java_import(
            **kwargs
        )

    return struct(
        java_library = java_library,
        java_binary = java_binary,
        java_test = java_test,
        java_import = java_import,
    )
