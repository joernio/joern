# Create and query a Code Property Graph (CPG)

## Building

As prerequisites to building please install: 

* Python3
  - Link: https://www.python.org/downloads/
* Java runtime 8
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/
  
With those dependencies installed you run `./build.sh` which will
build fuzzyc2cpg and the Joern program used to query a CPG.

## CPG Creation

The CPG is an open and language agnostic
[format](https://github.com/ShiftLeftSecurity/codepropertygraph#base-schema-for-the-code-property-graph)
, you can either provide your own CPG or use our
open source C/C++ frontend [fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg)
to create a CPG for any C/C++ program.

Run `./fuzzyc2cpg.sh -i <path/to/directory> -o <path/to/cpg>` in order generate a CPG.

## CPG Querying

Run `./joern.sh <path/to/cpg>` to query the CPG.
By default Joern only queries the CPG for a all methods defined in the
CPG but you can run your own queries by modifing
[src/main/scala/io/shiftleft/Main.scala], rebuilding and executing Joern
again.
