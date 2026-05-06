# abap2cpg Architecture

## Overview

abap2cpg converts ABAP (SAP's Advanced Business Application Programming Language) source files into a Code Property Graph (CPG) for analysis with Joern/Ocular. It follows the standard Joern x2cpg pattern: a language-specific parser produces JSON, which a Scala frontend transforms into CPG nodes via a series of passes.

## Pipeline

```
ABAP source files
      │
      ▼
┌─────────────────────┐
│   abapgen binary    │  (Node.js, bundled per platform)
│   parse-abap.js     │  uses @abaplint/core
└─────────────────────┘
      │  one JSON file per .abap input
      ▼
┌─────────────────────┐
│  AbapJsonParser     │  JSON → Intermediate AST
│  (Scala)            │  (ProgramRoot, MethodDef, …)
└─────────────────────┘
      │
      ▼
┌─────────────────────┐
│  AstCreator         │  Intermediate AST → CPG nodes
│  (Scala)            │  (Method, Call, Identifier, …)
└─────────────────────┘
      │
      ▼
┌───────────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────────────────┐  ┌──────────────┐  ┌─────────────┐
│ ContainsEdgePass  │→ │ TypeNodePass │→ │ RefEdgePass  │→ │ AbapTypeInferencePass  │→ │ TypeEvalPass │→ │  CPG (.bin) │
└───────────────────┘  └──────────────┘  └──────────────┘  └────────────────────────┘  └──────────────┘  └─────────────┘
```

## Components

### Stage 1 — ABAP Parser (`abapgen` binary)

- Written in Node.js (~40 lines), uses [@abaplint/core](https://github.com/abaplint/abaplint) to parse ABAP source.
- Lives in the [joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo) repository under `abap-astgen/parse-abap.js`; published as self-contained native binaries per platform (`abapgen-{linux-x64,linux-arm64,macos-x64,macos-arm64,win-x64.exe}`).
- Does **no interpretation** — dumps a flat list of raw statements (type + tokens + position) per file.
- `build.sbt` downloads the binaries from a pinned astgen-monorepo release into `bin/astgen/`.
- JSON schema is documented in [`parser-output.md`](parser-output.md).

This follows the same pattern as `jssrc2cpg`'s `astgen`: the JS layer only does what `@abaplint/core` uniquely provides (ABAP grammar + tokenisation); all semantic understanding lives in Scala where it is testable.

### Stage 2 — Scala Frontend

#### `AbapAstGenRunner`
Selects the correct platform binary, invokes it on the input directory, parses stdout for `OK`/`ERR` status lines, and returns the list of successfully parsed JSON files.

#### `AbapJsonParser`
Reads each JSON file and produces a `ProgramRoot` via a **state machine** over the flat statement list. Key responsibilities:
- Grouping statements into class definitions, method signatures, and method bodies by tracking `ClassDefinition` / `ClassImplementation` / `MethodImplementation` / `EndMethod` boundaries.
- Handling multiple CLASS DEFINITION/IMPLEMENTATION blocks per file (e.g. `locals_imp` files with many local classes).
- Parsing method parameter signatures from `MethodDef` token streams.
- Parsing body statements from token streams (`parseMoveStatement`, `parseCallStatement`, `parseDataDeclaration`, etc.).
- Merging collected signatures with collected bodies by method name after the full statement list is processed.

#### `AstCreator` (trait-based architecture)
Walks the intermediate AST and emits CPG nodes into a `DiffGraphBuilder`. Extends `AstCreatorBase[AbapNode, AstCreator]` from x2cpg. Following the April 2026 refactoring, AstCreator is now organized using trait composition:

- **AstCreator** (main orchestrator, 126 lines) — Composes all traits and orchestrates CPG creation
- **AstHelpers** — Code generation utilities (`codeFromExpr`, `operatorCode`)
- **AstForParametersCreator** — Creates parameter and local variable nodes
- **AstForExpressionsCreator** — Dispatcher for all expression types
  - **AstForSimpleExpressionsCreator** — Identifiers, literals
  - **AstForCallsCreator** — Method calls, operator calls, field access
- **AstForStatementsCreator** — Assignments, data declarations
- **AstForDeclarationsCreator** — Method and type declarations

The main `AstCreator` manages a `VariableScopeManager` for tracking declared variables per method scope and delegates to specialized traits for node creation.

#### Pass execution order (in `Abap2Cpg.createCpg`)

| Order | Pass | Purpose |
|-------|------|---------|
| 1 | `MetaDataPass` | Records language = ABAP |
| 2 | `AstCreationPass` | Parallel creation of FILE, NAMESPACE_BLOCK, TYPEDECL, METHOD, CALL, … nodes |
| 3 | `ContainsEdgePass` | Adds `CONTAINS` edges (x2cpg built-in) |
| 4 | `TypeNodePass.withTypesFromCpg` | Creates TYPE nodes from type full names (x2cpg built-in) |
| 5 | `RefEdgePass` | Creates `REF` edges from IDENTIFIER to LOCAL/PARAMETER |
| 6 | `AbapTypeInferencePass` | Propagates inferred type information across nodes |
| 7 | `TypeEvalPass` | Resolves `TYPE_FULL_NAME` references to TYPE nodes (x2cpg built-in) |

## Key Design Decisions

**Two-stage architecture** — The ABAP grammar is complex and already well-handled by `@abaplint/core`. Delegating lexing/parsing to Node.js keeps the Scala code focused on CPG semantics.

**Platform-bundled binary** — `@yao-pkg/pkg` (in astgen-monorepo) bundles the Node.js runtime and script into a single executable per target platform. The runner selects the right one at startup. Binaries are not vendored here; `build.sbt` downloads them from astgen-monorepo releases into `bin/astgen/`.

**Intermediate AST** — Rather than converting JSON directly to CPG nodes, an intermediate representation (`AbapIntermediateAst`) decouples JSON parsing from CPG generation. This makes both layers independently testable. See [`intermediate-ast.md`](intermediate-ast.md).

**Flat statement grouping** — `@abaplint/core` emits all statements in source order without nesting. `AbapJsonParser` uses a state machine to group them: `ClassDefinition` opens a class scope, `MethodDef` tokens within it yield parameter signatures, `MethodImplementation` opens a body scope, body statements accumulate until `EndMethod`, and finally signatures and bodies are merged by name. This handles multiple classes per file naturally.

**Variable scoping for REF edges** — `AstCreator` registers every local/parameter with `VariableScopeManager` as it processes declarations. After all nodes are created, `scope.createVariableReferenceLinks` and then `RefEdgePass` connect `IDENTIFIER` nodes to their declarations, enabling dataflow analysis.

**Receiver nodes on call sites** — For every call with a non-empty, non-placeholder target (`lo_obj->method()`, `CL_CLASS=>method()`), `AstCreator.astForCall` creates an IDENTIFIER node for the receiver and passes it as the `base` argument to `callAst`. This places it at `argumentIndex = 0` and wires both ARGUMENT and RECEIVER edges automatically. The receiver IDENTIFIER participates in REF linking, so dataflow through object receivers works when the object is a declared variable.

**Operator calls are never class-qualified** — `methodFullName` for calls whose `methodName` starts with `<operator>` is always the bare operator string, regardless of which class the call is inside. This prevents `<operator>.assignment` from becoming `MyClass::<operator>.assignment`.

**Trait-based architecture** — Following the April 2026 refactoring, AstCreator was split from a monolithic 567-line file into focused, composable traits. This follows the patterns established by `javasrc2cpg` and `pysrc2cpg`, improving maintainability and testability. The main AstCreator is now 126 lines and composes specialized traits for expressions, statements, declarations, and helpers.

## Source Layout

```
joern-cli/frontends/abap2cpg/
├── bin/astgen/                          # Platform binaries (downloaded by build.sbt from astgen-monorepo)
├── build.sbt                            # Scala build; downloads abapgen binaries
├── abap2cpg.sh                          # Shell runner wrapper
└── src/
    ├── main/scala/io/joern/abap2cpg/
    │   ├── Main.scala                   # CLI entry point (X2CpgMain)
    │   ├── Abap2Cpg.scala              # Frontend orchestrator
    │   ├── parser/
    │   │   ├── AbapAstGenRunner.scala   # Binary selection & invocation
    │   │   ├── AbapIntermediateAst.scala # Sealed AST node hierarchy
    │   │   └── AbapJsonParser.scala     # JSON → Intermediate AST
    │   ├── astcreation/
    │   │   ├── AstHelpers.scala         # Code generation utilities
    │   │   ├── declarations/
    │   │   │   ├── AstForDeclarationsCreator.scala  # Methods, types
    │   │   │   └── AstForParametersCreator.scala    # Parameters, locals
    │   │   ├── expressions/
    │   │   │   ├── AstForExpressionsCreator.scala   # Expression dispatcher
    │   │   │   ├── AstForCallsCreator.scala         # Call expressions
    │   │   │   └── AstForSimpleExpressionsCreator.scala  # Identifiers, literals
    │   │   └── statements/
    │   │       └── AstForStatementsCreator.scala    # Assignments, declarations
    │   └── passes/
    │       ├── AstCreationPass.scala          # Parallel CPG creation pass
    │       ├── AstCreator.scala               # Main orchestrator (trait composition)
    │       ├── RefEdgePass.scala              # REF edges for dataflow
    │       ├── AbapTypeInferencePass.scala    # Propagates inferred types
    │       └── NamespacePass.scala            # NAMESPACE nodes
    └── test/scala/io/joern/abap2cpg/
        ├── passes/AbapIntegrationTests.scala
        ├── passes/AstCreatorTests.scala
        ├── passes/SecurityStatementTests.scala
        └── testfixtures/
            ├── Abap2CpgFrontend.scala   # LanguageFrontend wrapper
            ├── Abap2CpgSuite.scala      # Code2CpgFixture wired to abapgen
            └── AbapCpgFixture.scala     # Binary-free unit test fixture
```

## Testing

Two tiers of tests cover the frontend independently.

### Unit tests — `AstCreatorTests` (`AbapCpgFixture`)

No binary required. `AbapCpgFixture` constructs `ProgramRoot` objects directly in Scala and runs:

```
AstCreator → ContainsEdgePass → TypeNodePass → NamespacePass → RefEdgePass
```

Helper builders:
- `programWithMethod(name, importing, exporting, changing, returning, body)` — standalone method
- `programWithClass(className, methodName, importing, body)` — class with one method

Coverage: method names/fullNames, parameter names/types/indices/evaluationStrategy, signatures, file/namespace/TypeDecl nodes, call dispatch types, `methodFullName` resolution, operator calls (using `Operators` constants from CPG schema), REF edges to locals and parameters.

### Integration tests — `AbapIntegrationTests` (`Abap2CpgSuite`)

Requires the `abapgen` binary. Writes a real ABAP source file to a temp directory and runs the full pipeline end-to-end. The fixture class uses the `.clas.abap` file suffix, which is required by abapgen's naming convention check.

Test subject: a `zcl_simple` class with three methods — `greet` (IMPORTING `iv_name`, RETURNING `rv_result`), `add` (IMPORTING `iv_a iv_b`, RETURNING `rv_sum`), `run` (IMPORTING `iv_input`).

Coverage: everything in unit tests plus parser correctness, ABAP keyword filtering from method/param names, `me->run` → `DYNAMIC_DISPATCH` / `me.run`, `zcl_simple=>greet` → `STATIC_DISPATCH` / `zcl_simple.greet`, REF edges from real parsed identifiers, language metadata.

### Running the tests

```bash
sbt "abap2cpg/test"                         # all tests (requires abapgen binary)
sbt "abap2cpg/testOnly *.AstCreatorTests"   # unit tests only, no binary needed
```

## Building

```bash
# Compile and package the Scala frontend
# (build.sbt downloads prebuilt abapgen binaries from astgen-monorepo releases)
sbt abap2cpg/stage      # produces target/universal/stage/

# Run
./abap2cpg.sh <input-dir> -o cpg.bin
```

To rebuild the parser itself, work in [joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo) under `abap-astgen/` and publish a new release; the pinned version here lives in `build.sbt`.

## Security Analysis Coverage

The CPG structure supports ABAP-specific security analysis by downstream tooling. Security queries themselves live outside this repository. The CPG exposes:

**Dangerous function/method calls** — all `CALL FUNCTION 'name'` and `object->method()` calls produce CALL nodes queryable by name. Key names:
`SXPG_COMMAND_EXECUTE`, `RFC_REMOTE_EXEC`, `RFC_REMOTE_PIPE`, `RFC_REMOTE_FILE`, `FTP_CONNECT`, `C_RSTRB_READ_BUFFERED`, `get_persistent_by_query`, `execute_procedure`, `cl_gui_frontend_services.execute`

**ABAP-specific statement CALL nodes** — `parseBodyStatement` maps these abaplint statement types to named CALL nodes:
`OPEN_DATASET`, `READ_DATASET`, `DELETE_DATASET`, `TRANSFER`, `AUTHORITY_CHECK`, `GENERATE_SUBROUTINE_POOL`, `CALL_TRANSFORMATION`, `EDITOR_CALL`, `DO_TIMES`

**Literals** — `STRING` and `NUMBER` typed LITERAL nodes for hardcoded values, string templates (`|...|`), and quoted strings (`'...'`).

**Dataflow** — `REACHING_DEF` edges from the `ReachingDefPass` overlay enable taint tracking from sources (e.g. `request->get_form_field`) to sinks.

## CPG Specification

The output graph must conform to the CPG 1.1 spec:
- Website: https://cpg.joern.io/
- Schema source: `codepropertygraph/schema/src/main/scala/io/shiftleft/codepropertygraph/schema/`

Key schema files relevant to abap2cpg: `Ast.scala` (node types), `CallGraph.scala` (dispatch/argument fields), `Operators.scala` (standard operator full names), `Method.scala` (method/parameter nodes), `Base.scala` (shared properties like `FULL_NAME`, `CODE`, `ORDER`).

See `cpg-nodes.md` for where abap2cpg currently deviates from the spec.

## Dependencies

| Layer | Dependency | Purpose |
|-------|-----------|---------|
| Node.js (external, in [astgen-monorepo](https://github.com/joernio/astgen-monorepo)) | `@abaplint/core` | ABAP grammar + AST |
| Node.js (external) | `@yao-pkg/pkg` | Bundle Node.js into native binary |
| Scala | `x2cpg` | Frontend framework, pass base classes |
| Scala | `codepropertygraph` | CPG node/edge types |
| Scala | `dataflowengineoss` | Dataflow analysis engine |
| Scala | `ujson` 4.1.0 | JSON parsing |
