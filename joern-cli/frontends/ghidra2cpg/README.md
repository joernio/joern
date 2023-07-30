
# Ghidra2cpg

This is a [CPG](https://docs.joern.io/code-property-graph/) frontend based on [Ghidra](https://ghidra-sre.org/). 

[![Build Status](https://github.com/joernio/ghidra2cpg/workflows/release/badge.svg)](https://github.com/joernio/ghidra2cpg/actions?query=workflow%3Arelease)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.joern/ghidra2cpg_3/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.joern/ghidra2cpg_3)

## Setup

Requirements:
 - We recommend openjdk 11 (at least). It does not work with adoptopenjdk 16. 
 - sbt (https://www.scala-sbt.org/)

### Known issues
`varags` are not handled properly: https://github.com/NationalSecurityAgency/ghidra/issues/234

