def _filter_runfiles_impl(ctx):
    target = ctx.attr.target
    default_info = target[DefaultInfo]
    runfiles = default_info.default_runfiles

    filtered = depset([
        file for file in runfiles.files.to_list()
        if not any([file.short_path.startswith(path) for path in ctx.attr.exclude_prefixes])
    ])

    new_runfiles = ctx.runfiles(
        transitive_files = filtered,
    )

    return [DefaultInfo(
        files = default_info.files,
        runfiles = new_runfiles,
    )]

filter_runfiles = rule(
    implementation = _filter_runfiles_impl,
    attrs = {
        "target": attr.label(),
        "exclude_prefixes": attr.string_list(default = []),
    },
)
