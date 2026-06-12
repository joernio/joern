# Rust goastgen

This workspace contains the Rust implementation of the `goastgen` binary used by
`gosrc2cpg`. It preserves the existing native frontend contract:

```bash
goastgen [-exclude <regex>] [-include-packages <regex>] -out <dir> <input>
goastgen -version
```

The Scala frontend still owns CPG construction. This binary only parses Go
source and `go.mod` files and emits the JSON shape consumed by
`GoAstJsonParser`, `ParserAst`, and `GoAstGenRunner`.

## Local Development

Run Rust checks:

```bash
cd joern-cli/frontends/gosrc2cpg/rust
cargo test
```

Build and install the Rust binary into the location used by the existing
`gosrc2cpg` SBT project:

```bash
sbt 'gosrc2cpg/goAstGenBuildRust'
```

Then run Go frontend tests. The dependency-download suite also requires the Go
toolchain on `PATH` because the Scala dependency pass runs `go mod` commands.

```bash
sbt 'gosrc2cpg/test'
```

## Release Artifacts

The Joern SBT packaging still expects the legacy artifact names:

- `goastgen-windows.exe`
- `goastgen-linux`
- `goastgen-linux-arm64`
- `goastgen-macos`
- `goastgen-macos-arm64`

Publish Rust builds with those exact names under the existing
`joernio/goastgen` release URL pattern. No Scala runtime changes are needed when
the artifact names and `goastgen -version` stay compatible with
`gosrc2cpg.goastgen_version`.

## Rollback

Rollback is the same as the legacy frontend: publish or point
`gosrc2cpg.goastgen_version` back to a release containing the previous Go
binary artifacts. The Scala frontend will continue to use either a compatible
`goastgen` on `PATH` or the bundled binary under `bin/astgen`.
