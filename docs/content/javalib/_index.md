+++
title="Using Joern as a Java Library"
weight=4
+++

Joern and Ocular offer a Java API that can be used from JVM-based languages such as Java, Scala, Groovy, Jython, or Kotlin. This interface is best if you are looking to build a standalone analyzer based on Joern and need control over graph loading and task scheduling. In comparison to the REST API, this API provides greater type safety and does not require communication with a local HTTP server. Instead, Joern can be fully embedded in your application.

For code property graph creation, Joern includes
[fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg), a fuzzy
C/C++ language module. The following Scala test code demonstrates how FuzzyC2CPG can be used to create a code property graph from all C/C++ files in the directory `joern-cli/src/test/resources/testcode/free` and save it at `/tmp/cpg.bin.zip`.

{{<snippet file="src/test/scala/io/shiftleft/joern/GenerationTests.scala" language="scala">}}

For your convenience, Joern includes two thin test programs around this API, `joern-parse` and `joern-query`, which allow creating and querying CPGs respectively.
