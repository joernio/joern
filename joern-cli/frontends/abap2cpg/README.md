# abap2cpg - ABAP Code Property Graph Frontend

An ABAP to CPG (Code Property Graph) converter for Joern.

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


## Module Structure

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
