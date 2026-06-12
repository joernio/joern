Oxidized Joern
===

Oxidized Joern is a Rust-first fork of
[Joern](https://github.com/joernio/joern), the code analysis workbench built
around code property graphs (CPGs).

The goal of this fork is to migrate Joern's frontend and analysis pipeline
toward Rust while preserving the Joern user model: parse source code, emit CPGs,
and query them with the existing Joern tooling. This repository is not the
upstream Joern distribution; it is the rewrite track where Rust replacements are
landed, tested, and released incrementally.

## Current Status

The first released Rust rewrite component is the Go AST generator used by
`gosrc2cpg`.

- Rust `goastgen` release: `go-astgen/v0.2.0`
- Release page:
  `https://github.com/allsmog/oxidized-joern/releases/tag/go-astgen/v0.2.0`
- Release assets:
  `goastgen-linux`, `goastgen-linux-arm64`, `goastgen-macos`,
  `goastgen-macos-arm64`, and `goastgen-windows.exe`
- Scala `gosrc2cpg` integration is wired to download from this fork's release
  URL.

The C/C++ frontend is still inherited from upstream Joern's Scala/Eclipse CDT
implementation in this checkout. Treat a Rust C/C++ frontend as roadmap work
unless a future branch or release explicitly lands it.

## Rewrite Priorities

1. Keep the upstream Joern CLI and CPG behavior usable while replacing
   implementation pieces with Rust.
2. Replace language-frontends behind stable contracts before changing the user
   interface.
3. Verify every Rust replacement with compatibility tests, differential tests,
   real corpus tests, and release artifact smoke tests.
4. Move from component rewrites toward a Rust-native core only after frontend
   compatibility is boring and repeatable.

## Go Frontend / `goastgen`

The Rust `goastgen` workspace lives under:

```text
joern-cli/frontends/gosrc2cpg/rust
```

Useful commands:

```bash
cd joern-cli/frontends/gosrc2cpg/rust
cargo fmt --check
cargo test
cargo clippy --all-targets -- -D warnings
```

Run the optional legacy differential test when a reference `goastgen` binary is
available:

```bash
GOASTGEN_REFERENCE=/path/to/legacy/goastgen \
cargo test -p goastgen --test differential_json -- --nocapture
```

Run the Scala Go frontend suite against the locally built Rust binary:

```bash
sbt 'gosrc2cpg/goAstGenBuildRust' 'gosrc2cpg/test'
```

Release notes and compatibility details are in:

- [`joern-cli/frontends/gosrc2cpg/rust/README.md`](joern-cli/frontends/gosrc2cpg/rust/README.md)
- [`joern-cli/frontends/gosrc2cpg/rust/docs/json-contract.md`](joern-cli/frontends/gosrc2cpg/rust/docs/json-contract.md)
- [`joern-cli/frontends/gosrc2cpg/rust/docs/release-checklist.md`](joern-cli/frontends/gosrc2cpg/rust/docs/release-checklist.md)

## Requirements

- JDK 21
- sbt
- Rust stable toolchain
- Go toolchain for Go dependency-resolution integration tests
- Optional: gcc and g++ for upstream C/C++ system-header discovery

## Upstream Joern

Joern remains the base project and compatibility target.

- Website: https://joern.io
- Documentation: https://docs.joern.io
- CPG specification: https://cpg.joern.io
- Upstream repository: https://github.com/joernio/joern

When in doubt, preserve upstream behavior first and make Rust-specific behavior
explicit in this fork's docs and tests.
