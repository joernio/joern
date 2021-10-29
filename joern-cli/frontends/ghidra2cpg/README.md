
# Ghidra2cpg

This is a [CPG](https://docs.joern.io/code-property-graph/) frontend based on [Ghidra](https://ghidra-sre.org/). 

[![Build Status](https://github.com/joernio/ghidra2cpg/workflows/release/badge.svg)](https://github.com/joernio/ghidra2cpg/actions?query=workflow%3Arelease)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.joern/ghidra2cpg_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.joern/ghidra2cpg_2.13)

## Setup

Requirements:
 - We recommend openjdk 11 (at least). It does not work with adoptopenjdk 16. 
 - sbt (https://www.scala-sbt.org/)

### Quickstart

1. Clone the project
2. Build the project `sbt stage`
3. Create a CPG `./ghidra2cpg.sh /path/to/your/binary -o /path/to/cpg.bin`
4. Download joern with
   ```
   wget https://github.com/joernio/joern/releases/download/v1.1.164/joern-cli.zip
   unzip joern-cli.zip
   cd joern-cli
   ```
5. Copy `cpg.bin` into the joern directory
6. Start joern with `./joern.sh`
7. Import the cpg with `importCpg("cpg.bin")`
8. Now you can query the CPG 

### Known issues
`varags` are not handled properly: https://github.com/NationalSecurityAgency/ghidra/issues/234

