# goastgen Fixture Corpus

These fixtures are source inputs for the Rust `goastgen` contract and
differential tests. They are intentionally small and focused so JSON and CPG
drift is easy to triage.

Current fixture groups:

- `build_tags`: build constraints and platform-specific source files.
- `cgo`: cgo preamble and import shape.
- `language_features`: generics, interfaces, select/send/go/defer, type
  assertions, function literals, and range statements.
- `module_layout`: root and nested `go.mod` files, grouped dependencies,
  indirect dependencies, ignored replace directives, `go.work` presence,
  vendored source trees, and package subfolders.

Run the opt-in differential harness against an explicit reference binary:

```bash
cd joern-cli/frontends/gosrc2cpg/rust
GOASTGEN_REFERENCE=/path/to/reference/goastgen cargo test -p goastgen --test differential_json -- --nocapture
```

The harness writes both reference and Rust outputs to temporary directories and
compares normalized JSON. It is skipped by default because this repository does
not track the legacy native `goastgen` binary.

For release hardening, add real checked-out projects with `GOASTGEN_REAL_CORPUS`
using the platform path-list separator:

```bash
GOASTGEN_REFERENCE=/path/to/reference/goastgen \
GOASTGEN_REAL_CORPUS="/path/to/repo1:/path/to/repo2:/path/to/repo3" \
cargo test -p goastgen --test differential_json -- --nocapture
```

Malformed-source behavior is covered by the CLI contract tests rather than this
corpus: parse-error inputs should be reported as skipped files and should not
produce JSON output.

When a reference release is selected, generated golden JSON can be promoted from
the harness output into a checked-in `fixtures/golden/<version>/` directory.
