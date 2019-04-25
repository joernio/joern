+++
title="Building the Code"
weight=1
+++

Joern combines the following components to provide a fully functional code analysis platform for C/C++:

* FuzzyC2CPG fuzzy parser for C/C++: https://github.com/ShiftLeftSecurity/fuzzyc2cpg
* ShiftLeft Tinkergraph: https://github.com/ShiftLeftSecurity/tinkergraph-gremlin
* Semantic code property graph and query language: https://github.com/ShiftLeftSecurity/codepropertygraph

These components are automatically fetched during the build and do
*NOT* have to be installed manually. Instead, all that is required are
the following three dependencies:

* Python3
  - Link: https://www.python.org/downloads/
* Java runtime 8
  - Link: http://openjdk.java.net/install/
* Scala build tool (sbt)
  - Link: https://www.scala-sbt.org/

Python3 and Java 8 are present in the official package repositories of
all major current Linux distributions and BSD flavors. For installing
the the Scala build tool (sbt), please following the instructions at:

https://www.scala-sbt.org/download.html

Any 1.x version of sbt works as sbt downloads the correct version for
building joern as part of the build process.

Once the dependencies are installed, run
```
./build.sh
```
This will build both the C/C++ language frontend `fuzzyc2cpg` and the
`codepropertygraph`, along with its query language.

To test if the build was successful, you can run
```
./fuzzyc2cpg.sh tests/free
```
This command will create a code property graph for the sample program
in the directory `tests/free`, and store the graph in the file
`cpg.bin.zip`.