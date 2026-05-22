load("@rules_java//java:defs.bzl",
   _java_binary = "java_binary",
   _java_library = "java_library",
   _java_test = "java_test",
   _java_import = "java_import")

def make_java_rules(java_common_opts):
    def java_binary(javacopts = [], **kwargs):
        _java_binary(
          javacopts = java_common_opts + javacopts,
          **kwargs)

    def java_library(javacopts = [], **kwargs):
        _java_library(
          javacopts = java_common_opts + javacopts,
          **kwargs)

    def java_test(javacopts = [], **kwargs):
        _java_test(
          javacopts = java_common_opts + javacopts,
          **kwargs)

    def java_import(**kwargs):
        _java_import(
          **kwargs)

    return struct(
        java_library = java_library,
        java_binary = java_binary,
        java_test = java_test,
        java_import = java_import,
    )
