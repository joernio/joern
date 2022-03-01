# solidity2cpg

This is a [CPG](https://docs.joern.io/code-property-graph/) frontend for Solidity source code powered by
[Surya](https://github.com/ConsenSys/surya).

Surya outputs the Solidity contract AST as JSON which is then parsed using [Circe](https://circe.github.io/circe/).

[![Discord](https://img.shields.io/badge/-Discord-lime?style=for-the-badge&logo=discord&logoColor=white&color=black)](https://discord.com/invite/vv4MH284Hc)

## Setup

Requirements:

- \>= JDK 11. We recommend OpenJDK 11.
- sbt (https://www.scala-sbt.org/)

### Quickstart

1. Clone the project
2. Build the project `sbt stage`
3. Create a CPG `./solidity2cpg.sh /path/to/your/code -o /path/to/cpg.bin`
4. Download Joern with
   ```
   wget https://github.com/joernio/joern/releases/latest/download/joern-cli.zip
   unzip joern-cli.zip
   cd joern-cli
   ```
5. Copy `cpg.bin` into the Joern directory
6. Start Joern with `./joern.sh`
7. Import the cpg with `importCpg("cpg.bin")`
8. Now you can query the CPG

### Development

Some general development habits for the project:

- When making a branch, use the following template `<short-name>/<feature-or-bug-name>`
  e.g. `fabs/control-structure-nodes`.
- Code must be formatted using `sbt scalafmt` before a pull request otherwise the pipeline will fail.
