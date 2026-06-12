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

The JSON compatibility contract is documented in
[`docs/json-contract.md`](docs/json-contract.md). Keep that contract stable
until the Scala parser and CPG passes are intentionally migrated.

## Local Development

Run Rust checks:

```bash
cd joern-cli/frontends/gosrc2cpg/rust
cargo test
```

Run the opt-in differential JSON harness against an explicit reference
`goastgen` binary:

```bash
GOASTGEN_REFERENCE=/path/to/reference/goastgen cargo test -p goastgen --test differential_json -- --nocapture
```

Before cutting a release, include real checked-out Go projects in that same
diff by passing an OS path-list in `GOASTGEN_REAL_CORPUS`:

```bash
GOASTGEN_REFERENCE=/path/to/reference/goastgen \
GOASTGEN_REAL_CORPUS="/path/to/repo1:/path/to/repo2:/path/to/repo3" \
cargo test -p goastgen --test differential_json -- --nocapture
```

For local convenience, `GOASTGEN_RUN_DIFFERENTIAL=1` uses the host-specific
binary under `../bin/astgen` when present. The differential test is skipped by
default because legacy binaries are not tracked in this repository.

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

Publish Rust builds with those exact names under this fork's release URL
pattern:

```text
https://github.com/allsmog/joern/releases/download/go-astgen/v<version>/<artifact>
```

The `.github/workflows/goastgen-rust.yml` workflow publishes these artifacts
when a tag like `go-astgen/v0.2.0` is pushed. No Scala runtime changes are
needed when the artifact names and `goastgen -version` stay compatible with
`gosrc2cpg.goastgen_version`.

Use [`docs/release-checklist.md`](docs/release-checklist.md) before cutting a
tag. It includes the real-corpus differential, staged-artifact smoke, and
fork-download smoke commands.

## Rollback

Rollback is the same as the legacy frontend: publish or point
`gosrc2cpg.goastgen_version` back to a release containing the previous binary
artifacts. The Scala frontend will continue to use either a compatible
`goastgen` on `PATH` or the bundled binary under `bin/astgen`. The legacy Go
reference remains available at the upstream `joernio/astgen-monorepo`
`go-astgen/v0.1.0` release.
