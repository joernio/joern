# abap2cpg Architecture

## Overview

abap2cpg converts ABAP (SAP's Advanced Business Application Programming Language) source files into a Code Property Graph (CPG) for analysis with Joern/Ocular. It follows the standard Joern x2cpg pattern: a language-specific parser produces JSON, which a Scala frontend transforms into CPG nodes via a series of passes.

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

```bash
sbt "abap2cpg/test"                         # all tests (requires abapgen binary)
sbt "abap2cpg/testOnly *.AstCreatorTests"   # unit tests only, no binary needed
```
