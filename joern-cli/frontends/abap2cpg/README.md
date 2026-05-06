# abap2cpg - ABAP Code Property Graph Frontend

An ABAP to CPG (Code Property Graph) converter for Joern.

The Node.js-based ABAP parser (`abapgen`, built on [@abaplint/core](https://github.com/abaplint/abaplint)) lives in the [joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo) repository under `abap-astgen/`. This frontend consumes the prebuilt `abapgen` binaries and generates CPGs from their JSON output.

## Quick Start

```bash
# 1. Build the frontend (abapgen binaries are downloaded from astgen-monorepo by build.sbt)
sbt abap2cpg/stage

# 2. Generate CPG
./abap2cpg.sh /path/to/abap/sources -o cpg.bin

# 3. Analyze in Joern
joern
joern> importCpg("cpg.bin")
joern> cpg.call.name("AUTHORITY_CHECK").l  // Find authorization checks
joern> cpg.call.name(".*EXEC.*").l         // Find potential command injection
```

## Prerequisites

The build process has been verified on Linux and macOS. You will need:

* **Java runtime 11 or higher**
  - Link: http://openjdk.java.net/install/
  - Verify: `java -version`
  - Note: Java 13+ required for building (runtime check in build)

* **Scala build tool (sbt)**
  - Link: https://www.scala-sbt.org/
  - Verify: `sbt --version`

Node.js is **not** required to build this frontend — `build.sbt` downloads prebuilt `abapgen` binaries from the [astgen-monorepo](https://github.com/joernio/astgen-monorepo) releases. Node.js is only needed if you want to rebuild the parser yourself in that repository.

## Building the code

From the repository root:

```bash
sbt abap2cpg/stage
```

This will:
- Download the prebuilt `abapgen` binaries (linux-x64, linux-arm64, macos-x64, macos-arm64, win-x64) from the [astgen-monorepo](https://github.com/joernio/astgen-monorepo) release into `bin/astgen/`
- Compile the Scala sources
- Package the frontend for execution
- Create the launcher script and dependencies

The staged build will be available at `joern-cli/frontends/abap2cpg/target/universal/stage/`.

### Rebuilding the parser

If you need to modify the ABAP parser itself, clone [joernio/astgen-monorepo](https://github.com/joernio/astgen-monorepo), edit `abap-astgen/parse-abap.js`, run `npm run binary` to produce the native binaries, and publish a new release — `build.sbt` here picks up the binaries from the pinned release tag.

## Running

After building, you can generate a CPG from ABAP source code:

### Basic usage

```bash
./abap2cpg.sh <path/to/sourceCodeDirectory> --output <path/to/outputCpg>
```

### Examples

Parse a single ABAP file:
```bash
./abap2cpg.sh /path/to/program.abap -o cpg.bin
```

Parse a directory of ABAP files:
```bash
./abap2cpg.sh /path/to/abap/sources -o cpg.bin
```

### Command-line options

Run the following to see a complete list of available options:
```bash
./abap2cpg.sh --help
```

Common options include:
- `--output` / `-o`: Output CPG file path
- `--exclude`: Files or folders to exclude during CPG generation
- `--exclude-regex`: Regex pattern for files to exclude

## Usage Examples

### Analyzing ABAP Code in Joern

After generating a CPG, load it in Joern for analysis:

```bash
joern
```

```scala
// Load the CPG
importCpg("cpg.bin")

// Find all method calls
cpg.call.name.l

// Find potential SQL injection sinks
cpg.call.name(".*EXEC.*|.*QUERY.*").l
```

## Development

### Running Tests

```bash
# Run all tests
sbt "project abap2cpg" test

# Run specific test suite
sbt "project abap2cpg" "testOnly io.joern.abap2cpg.passes.AstCreatorTests"

# Run with detailed output
sbt "project abap2cpg" "testOnly io.joern.abap2cpg.passes.AstCreatorTests -- -oF"
```

**Current Test Coverage:**
- 149 tests, 100% passing
- 70 AstCreator tests (node creation, edges, properties)
- 47 Security statement tests
- 27 Integration tests
- 5 Parser tests


### Debugging

```bash
# Enable debug logging
export ABAP2CPG_DEBUG=1
./abap2cpg.sh /path/to/source -o cpg.bin

# Test the parser directly (from the astgen-monorepo checkout)
cd /path/to/astgen-monorepo/abap-astgen
node parse-abap.js /path/to/file.abap

# Inspect intermediate AST in sbt console
sbt "project abap2cpg" console
scala> import io.joern.abap2cpg.parser.*
scala> val parser = new AbapJsonParser()
scala> val result = parser.parseFile(Path.of("test.abap.json"))
```

## CPG Nodes and Edges

### Currently Generated Nodes

**File Structure:**
- `FILE` - Source file node
- `NAMESPACE_BLOCK` - Namespace block (global namespace)
- `NAMESPACE` - Namespace node

**Type Declarations:**
- `TYPE_DECL` - Class definitions
  - Properties: name, fullName, isExternal, filename
  - Line/column information from source

**Methods:**
- `METHOD` - Method definitions
  - Properties: name, fullName, signature, filename, isExternal
  - Line/column information
  - Signature format: `(param_types) -> return_type`

**Parameters:**
- `METHOD_PARAMETER_IN` - Input parameters (IMPORTING, CHANGING input)
  - Properties: name, code, typeFullName, evaluationStrategy, index, order
  - BY_VALUE or BY_REFERENCE evaluation strategy
- `METHOD_PARAMETER_OUT` - Output parameters (EXPORTING, CHANGING output)
  - Same properties as PARAMETER_IN
- `METHOD_RETURN` - Return value (RETURNING parameter)
  - Properties: evaluationStrategy, typeFullName, code

**Variables:**
- `LOCAL` - Local variables (DATA declarations)
  - Properties: name, code, typeFullName, order
  - Line/column information

**Blocks:**
- `BLOCK` - Method body blocks
  - typeFullName: "void"

**Expressions:**
- `IDENTIFIER` - Variable/parameter references
  - Properties: name, code, typeFullName, argumentIndex, order
  - Line/column information
- `LITERAL` - Literal values (strings, numbers)
  - Properties: code, typeFullName, argumentIndex, order
  - typeFullName: "STRING", "NUMBER", etc.
- `FIELD_IDENTIFIER` - Field identifiers in field access operations
  - Properties: canonicalName, code, argumentIndex, order

**Calls:**
- `CALL` - Method calls and operators
  - Properties: name, code, methodFullName, typeFullName, dispatchType, order
  - dispatchType: STATIC_DISPATCH or DYNAMIC_DISPATCH
  - Operators: assignment, fieldAccess, indirectFieldAccess, addition, subtraction, multiplication, division, indirection
  - Line/column information

### Currently Generated Edges

**AST Structure:**
- `AST` - Tree structure edges connecting parents to children
  - ORDER property for sibling ordering

**Arguments:**
- `ARGUMENT` - Connects CALL to argument expressions
  - Properties: argumentIndex (1-based), argumentName (for named parameters)

**Receiver:**
- `RECEIVER` - Connects CALL to receiver object (for instance methods)
  - argumentIndex: 0

**Variable References:**
- `REF` - Connects IDENTIFIER to declaration (LOCAL, PARAMETER_IN, etc.)
  - Created by RefEdgePass

**Control Flow:**
- `CFG` - Control flow edges between statements
  - Basic sequential flow only (no branching yet)

### Not Yet Generated Nodes

**Control Flow:**
- `CONTROL_STRUCTURE` - IF, LOOP, CASE, TRY blocks
- `JUMP_TARGET` - Labels for jumps
- `UNKNOWN` - Fallback for unrecognized constructs

**Type System:**
- `TYPE` - Type definitions
- `TYPE_PARAMETER` - Generic type parameters
- `TYPE_ARGUMENT` - Instantiated type arguments

**Annotations:**
- `ANNOTATION` - ABAP annotations
- `ANNOTATION_PARAMETER` - Annotation parameters

**Comments:**
- `COMMENT` - Code comments

### Not Yet Generated Edges

**Type Information:**
- `EVAL_TYPE` - Type evaluation results
- `INHERITS_FROM` - Class inheritance relationships
- `BINDS_TO` - Interface implementation

**Control Flow:**
- `TRUE` / `FALSE` - Conditional branch edges
- `REACHING_DEF` - Reaching definitions

**Dataflow:**
- Currently relies on built-in dataflow engine
- No custom dataflow edges

**Call Graph:**
- Currently relies on built-in call graph construction
- No custom call edges

### Possible Future Extensions

**Additional Nodes:**
- `MEMBER` - Class fields/attributes
- `MODIFIER` - Access modifiers (PUBLIC, PRIVATE, PROTECTED)
- `ARRAY_INITIALIZER` - Internal table initializations
- `CLOSURE_BINDING` - Captures for nested contexts

**Additional Edges:**
- `CONTAINS` - Containment relationships
- `SOURCE_FILE` - Links to source file
- `VTABLE` - Virtual method table for inheritance
- `ALIAS` - Aliasing relationships

### Known Limitations

1. **No control flow structures** - IF, LOOP, CASE not yet implemented
2. **Limited type inference** - Using "ANY" placeholder for all types
3. **No class inheritance** - INHERITS_FROM edges not created
4. **No ABAP SQL** - SELECT, INSERT, UPDATE not parsed yet
5. **Basic operators only** - Missing logical and comparison operators

## Contributing

Contributions welcome! Priority areas:

**High Priority:**
- Control flow structures (IF, LOOP, CASE, TRY)
- Type inference system
- ABAP SQL support (SELECT, INSERT, UPDATE, DELETE)

**Medium Priority:**
- Class inheritance (INHERITS_FROM edges)
- Internal tables operations (LOOP AT, READ TABLE)
- More operator support (logical, comparison, string)

**Low Priority:**
- Interface implementation (BINDS_TO edges)
- Screen fields and events
- ALV operations

## Resources

**ABAP Documentation:**
- [Official ABAP Documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm)
- [@abaplint/core](https://github.com/abaplint/abaplint) - Parser library
- [ABAP Language Reference](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap.htm)

## How it works

The abap2cpg frontend operates in two stages:

### Stage 1: AST Generation (Node.js)

The `parse-abap.js` script uses [@abaplint/core](https://github.com/abaplint/abaplint) to parse ABAP source files and extract:
- **Statement-level AST information** - All statements with their types and tokens
- **Method definitions** - Class methods (static/instance) with parameters
- **Method implementations** - Method bodies with all contained statements
- **FORM routines** - Classic ABAP subroutines with USING/CHANGING/TABLES parameters
- **Function modules** - ABAP function modules

The parser outputs structured JSON containing:
- Tokens for each statement
- Position information (row/column for start/end)
- Method metadata (name, parameters, types, modifiers)
- Nested body structures for methods and forms

### Stage 2: CPG Generation (Scala)

The Scala-based CPG generator processes the JSON ASTs:
1. **AbapAstGenRunner** manages the parser execution and file processing
2. **AbapJsonParser** reads and validates the JSON output, producing an intermediate AST
3. **AstCreationPass** drives **AstCreator** (trait-composed) over the intermediate AST to emit CPG nodes/edges, following the [Code Property Graph specification](https://github.com/ShiftLeftSecurity/codepropertygraph)
4. Follow-up passes (`ContainsEdgePass`, `TypeNodePass`, `RefEdgePass`, `AbapTypeInferencePass`, `TypeEvalPass`) finish the graph

The generated CPG can then be analyzed using Joern/Ocular for:
- Security vulnerability detection
- Code quality analysis
- Data flow analysis
- Control flow analysis
- Pattern matching and custom queries

## Architecture

### Pipeline Overview

```
ABAP Source (.abap)
    ↓
[1] parse-abap.js (@abaplint/core)
    ↓
JSON AST (raw tokens + structure)
    ↓
[2] AbapJsonParser (Scala)
    ↓
Intermediate AST (typed nodes)
    ↓
[3] AstCreator (trait-based modules)
    ↓
Code Property Graph (.bin)
```

### Module Structure

The codebase follows a trait-based architecture (refactored April 2026):

```
abap2cpg/
├── parser/
│   ├── AbapAstGenRunner.scala      # Invokes Node.js parser
│   ├── AbapJsonParser.scala        # JSON → Intermediate AST
│   └── AbapIntermediateAst.scala   # AST data structures
│
├── astcreation/
│   ├── AstCreator.scala            # Main orchestrator (126 lines)
│   ├── AstHelpers.scala            # Code generation utilities
│   │
│   ├── declarations/
│   │   ├── AstForDeclarationsCreator.scala   # Methods, types
│   │   └── AstForParametersCreator.scala     # Parameters, locals
│   │
│   ├── expressions/
│   │   ├── AstForExpressionsCreator.scala    # Expression dispatcher
│   │   ├── AstForCallsCreator.scala          # Call expressions
│   │   └── AstForSimpleExpressionsCreator.scala  # Identifiers, literals
│   │
│   └── statements/
│       └── AstForStatementsCreator.scala     # Assignments, declarations
│
└── passes/
    ├── AstCreationPass.scala       # CPG creation pass
    ├── RefEdgePass.scala           # Variable reference edges
    └── NamespacePass.scala         # Namespace creation
```
