# abap2cpg

An ABAP to CPG (Code Property Graph) converter based on [@abaplint/core](https://github.com/abaplint/abaplint).

This frontend parses ABAP source code and generates code property graphs that can be analyzed using Joern/Ocular for security vulnerabilities, code quality issues, and architectural insights.

## Prerequisites

The build process has been verified on Linux and macOS. You will need:

* **Java runtime 11 or higher**
  - Link: http://openjdk.java.net/install/
  - Verify: `java -version`
  - Note: Java 13+ required for building (runtime check in build)

* **Scala build tool (sbt)**
  - Link: https://www.scala-sbt.org/
  - Verify: `sbt --version`

* **Node.js v18 or higher** (for building the ABAP parser)
  - Link: https://nodejs.org/
  - Verify: `node --version`
  - Required for: `npm install` and building native binaries with `pkg`

## Building the code

### Quick start (development)

For development, build only for your current platform:

```bash
cd joern-cli/frontends/abap2cpg
npm install
npm run build:current  # builds for your current platform only
cd ../../..
sbt abap2cpg/compile   # or sbt abap2cpg/stage
```

**Important:** Use `sbt abap2cpg/compile` (not `sbt compile` from root), as the root build triggers stage tasks that require all platform binaries.

### Detailed build steps

#### Step 1: Build the ABAP parser binaries

abap2cpg uses a Node.js-based parser that leverages @abaplint/core to generate abstract syntax trees.
Native binaries for Linux, macOS, and Windows are built using [pkg](https://github.com/vercel/pkg).

```bash
cd joern-cli/frontends/abap2cpg
npm install
npm run build
```

This will create the following binaries in `bin/astgen/`:
- `abapgen-linux` (Linux x64)
- `abapgen-linux-arm` (Linux ARM64)
- `abapgen-macos` (macOS x64)
- `abapgen-macos-arm` (macOS ARM64)
- `abapgen-win.exe` (Windows x64)

**Development tip:** Use `npm run build:current` to build only for your current platform (much faster).

#### Step 2: Build the CPG generator

From the repository root:

```bash
sbt abap2cpg/stage
```

This will:
- Compile the Scala sources
- Check that the required `abapgen` binaries exist in `bin/astgen/`
- Package the frontend for execution
- Create the launcher script and dependencies

The staged build will be available at `joern-cli/frontends/abap2cpg/target/universal/stage/`

### Building for distribution

To build for all platforms (e.g., for CI/CD or release):

```bash
cd joern-cli/frontends/abap2cpg
npm install
npm run build  # builds all platform binaries
cd ../../..
sbt abap2cpg/stage
```

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

## Supported ABAP Constructs

Currently supported:
- **Method definitions** (class methods and instance methods)
- **Method implementations** with body analysis
- **FORM routines** (subroutines)
- **Function modules**
- **Parameters** (IMPORTING, EXPORTING, CHANGING, RETURNING)
- **Statement-level analysis** with token information
- **Position tracking** (line and column information)

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
1. **AbapJsonParser** reads and validates the JSON output
2. **AbapAstGenRunner** manages the parser execution and file processing
3. **AstCreationPass** traverses the intermediate AST
4. **CpgGenerator** creates CPG nodes according to the [Code Property Graph specification](https://github.com/ShiftLeftSecurity/codepropertygraph)

The generated CPG can then be analyzed using Joern/Ocular for:
- Security vulnerability detection
- Code quality analysis
- Data flow analysis
- Control flow analysis
- Pattern matching and custom queries

## Architecture

```
ABAP Source (.abap)
    ↓
parse-abap.js (@abaplint/core)
    ↓
JSON AST
    ↓
AbapJsonParser (Scala)
    ↓
Intermediate AST
    ↓
AstCreationPass
    ↓
Code Property Graph (.bin)
```
