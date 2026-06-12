# goastgen JSON Contract

This document freezes the compatibility contract between the native `goastgen`
binary and the Scala `gosrc2cpg` pipeline.

The Rust implementation is expected to preserve this contract before any
Scala-side CPG construction changes are considered. The Scala consumer is
implemented by:

- `GoAstGenRunner.scala`, which invokes `goastgen`, discovers `*.json` files,
  records skipped files from stdout, and groups parsed files by Go module.
- `GoAstJsonParser.scala`, which reads each JSON document and loads the source
  file named by `node_filename`.
- `ParserAst.scala` and the `astcreation` package, which interpret `node_type`
  strings and well-known AST field names.

## CLI Contract

The binary must support the legacy forms used by `GoAstGenRunner`:

```bash
goastgen -out <output-dir> <input-path>
goastgen -exclude <regex> -out <output-dir> <input-path>
goastgen -include-packages <comma-or-regex> -out <output-dir> <input-path>
goastgen -version
```

`-out`, `-exclude`, `-include-packages`, and `-version` must remain accepted as
single-dash flags. Long `--flag` aliases are allowed, but they are not a
replacement for the legacy forms.

For each successfully parsed source file, stdout must include a line beginning
with `Converted`. `GoAstGenRunner.skippedFiles` treats any non-`Converted`
stdout line as a skipped-file report where the first token is the file name and
the remainder is the reason.

## File Layout

Given an input directory, the output directory must mirror the input tree and
append `.json` to each accepted input file name:

```text
input/main.go      -> output/main.go.json
input/go.mod       -> output/go.mod.json
input/pkg/a.go     -> output/pkg/a.go.json
```

Accepted inputs are Go source files and `go.mod` files. `go.work` files are
walked past but do not produce JSON, matching the legacy generator and current
Scala consumer. Vendored Go source files are treated like ordinary source files
and keep their relative output paths. Ignored files and excluded paths must not
produce JSON output.

## Source Positions

Every Go AST object consumed by Scala must include these common fields:

- `node_type`
- `node_id`
- `node_line_no`
- `node_line_no_end`
- `node_col_no`
- `node_col_no_end`

Line and column numbers are 1-based. `node_filename` must resolve to the
original source file on the `ast.File` root because `GoAstJsonParser.readFile`
opens it to attach source content to the parser result. Legacy Go source JSON
does not emit `node_filename` on nested AST nodes.

`node_reference_id` is optional and is used for reference stubs that point at a
previously emitted `node_id`. Exact numeric values only need to be stable within
one parsed file. The differential harness normalizes ID values but still checks
whether ID fields are present.

## Go Source Root

The root document for a Go source file is an `ast.File` object. Required fields:

- `Name`: package identifier as `ast.Ident`
- `Decls`: top-level declarations
- `Imports`: flattened import specs
- `Comments`: nullable legacy comment list
- `Unresolved`: array, possibly empty
- `GoVersion`: string, possibly empty
- `FileStart`, `FileEnd`, `Package`: source offsets
- `Scope`: nullable

## Go AST Node Type Inventory

The Scala side currently recognizes these `node_type` values through
`ParserAst.scala`:

- Expressions: `ast.BinaryExpr`, `ast.KeyValueExpr`, `ast.UnaryExpr`,
  `ast.StarExpr`, `ast.ParenExpr`, `ast.TypeAssertExpr`, `ast.SelectorExpr`,
  `ast.CallExpr`, `ast.StructType`, `ast.IndexExpr`, `ast.IndexListExpr`
- Statements: `ast.BlockStmt`, `ast.DeclStmt`, `ast.AssignStmt`,
  `ast.ExprStmt`, `ast.IncDecStmt`, `ast.IfStmt`, `ast.SwitchStmt`,
  `ast.TypeSwitchStmt`, `ast.ReturnStmt`, `ast.ForStmt`, `ast.RangeStmt`,
  `ast.BranchStmt`, `ast.DeferStmt`, `ast.GoStmt`, `ast.SelectStmt`,
  `ast.SendStmt`, `ast.LabeledStmt`
- Primitives and declarations: `ast.BasicLit`, `ast.Ident`,
  `ast.CompositeLit`, `ast.FuncLit`, `ast.File`, `ast.GenDecl`,
  `ast.ImportSpec`, `ast.FuncDecl`, `ast.ValueSpec`, `ast.CaseClause`,
  `ast.CommClause`, `ast.Field`, `ast.InterfaceType`, `ast.FuncType`,
  `ast.Ellipsis`, `ast.Unknown`, `ast.FieldList`, `ast.ArrayType`,
  `ast.MapType`, `ast.ChanType`, `ast.TypeSpec`

`tests/ast_coverage.rs` renders the fixture corpus, reads `ParserAst.scala`,
and fails when Rust emits a new Scala-unsupported `node_type` without an
explicit inventory entry. New node types should not be introduced until the
Scala parser and AST creation code are extended and covered by CPG tests, or
until the unsupported behavior is deliberately documented in that inventory.

## Field Names

The Scala side expects Go AST fields to use the legacy Go names, including:

`Assign`, `Body`, `Call`, `Chan`, `Comm`, `Cond`, `Decl`, `Decls`, `Else`,
`Init`, `Key`, `Kind`, `Label`, `List`, `Lhs`, `Name`, `Names`, `Obj`, `Op`,
`Path`, `Post`, `Rhs`, `Specs`, `Tag`, `Tok`, `Type`, `Value`, `Values`, `X`,
`Y`, `Results`, `Params`, `Elt`, `Sel`, `Elts`, `Fun`, `Fields`,
`TypeParams`, `Args`, `Recv`, `Index`, `Indices`, and `Methods`.

Absent optional fields and explicit `null` values are not always equivalent.
Match the legacy generator unless a Scala-side consumer has been audited.

## go.mod Contract

The `go.mod` JSON document is decoded as `GoMod` by upickle. It must include:

- `Module`, with `Name` and source position fields when a module declaration is
  present
- `dependencies`, an array of dependency objects

Each dependency object must include:

- `Module`
- `Version`
- `Indirect`

Additional fields are allowed only when the Scala model tolerates them.

## Compatibility Rules

The Rust implementation is compatible when:

- The same source corpus produces the same JSON file set as the reference
  generator.
- Shared JSON documents match after documented normalization of absolute paths.
- Existing `gosrc2cpg` tests pass when run against the Rust binary.
- Differential CPG tests show no unexplained changes in node types, edges,
  method full names, call graph edges, or dataflow query results.
