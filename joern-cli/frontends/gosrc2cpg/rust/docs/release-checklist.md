# goastgen Rust Release Checklist

Use this checklist before publishing a fork release such as
`go-astgen/v0.2.0`.

## Version And URL

- `joern-cli/frontends/gosrc2cpg/src/main/resources/application.conf` sets
  `gosrc2cpg.goastgen_version = "0.2.0"`.
- Rust `goastgen -version` prints `v0.2.0`.
- `joern-cli/frontends/gosrc2cpg/build.sbt` downloads artifacts from:

```text
https://github.com/allsmog/oxidized-joern/releases/download/go-astgen/v0.2.0/
```

## Required Local Gates

```bash
cd joern-cli/frontends/gosrc2cpg/rust
cargo fmt --check
cargo test
GOASTGEN_REFERENCE=/path/to/legacy/goastgen cargo test -p goastgen --test differential_json -- --nocapture
cargo clippy --all-targets -- -D warnings
```

`cargo test` includes `performance_envelope`, which runs a generated corpus
through the CLI and fails if it takes longer than 45 seconds. Override the local
threshold only when diagnosing slow machines:

```bash
GOASTGEN_PERF_ENVELOPE_SECONDS=90 cargo test -p goastgen --test performance_envelope -- --nocapture
```

Run Scala integration from the repository root:

```bash
sbt 'gosrc2cpg/scalafmtCheck' 'gosrc2cpg/goAstGenBuildRust' 'gosrc2cpg/test'
```

## Real Corpus Differential

The fixture corpus is necessary but not enough. Before tagging, run the
differential harness against at least three real checked-out Go projects:

```bash
cd joern-cli/frontends/gosrc2cpg/rust
GOASTGEN_REFERENCE=/path/to/legacy/goastgen \
GOASTGEN_REAL_CORPUS="/path/to/repo1:/path/to/repo2:/path/to/repo3" \
cargo test -p goastgen --test differential_json -- --nocapture
```

Any mismatch must be triaged as one of:

- Rust JSON compatibility bug.
- Intentional Scala-compatible divergence documented in `docs/json-contract.md`.
- Legacy reference identity difference that needs a normalizer update with a
  targeted fixture.

## Artifact Smoke

The GitHub workflow builds these exact release assets:

- `goastgen-windows.exe`
- `goastgen-linux`
- `goastgen-linux-arm64`
- `goastgen-macos`
- `goastgen-macos-arm64`

Each matrix build must run the staged artifact, verify `-version`, parse a tiny
Go package, and upload the artifact with the exact name above.

## Fork Release Smoke

After pushing the tag and waiting for the release workflow:

```bash
curl -L --fail -o /tmp/goastgen \
  https://github.com/allsmog/oxidized-joern/releases/download/go-astgen/v0.2.0/goastgen-macos-arm64
chmod +x /tmp/goastgen
/tmp/goastgen -version
```

Then verify the SBT downloader path from a clean cache:

```bash
rm -f joern-cli/frontends/gosrc2cpg/bin/astgen/goastgen-*
sbt 'gosrc2cpg/compile'
joern-cli/frontends/gosrc2cpg/bin/astgen/goastgen-macos-arm64 -version
```

Use the host-specific artifact name on Linux or Windows.
