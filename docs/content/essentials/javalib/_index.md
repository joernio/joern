+++
title="Using Joern as a Java Library"
weight=2
+++

Joern and Ocular offer a Java API that can be used from JVM-based languages such as Java, Scala, Groovy, Jython, or Kotlin. This interface is best if you are looking to build a standalone analyzer based on Joern and need control over graph loading and task scheduling. In comparison to the REST API, this API provides greater type safety and does not require communication with a local HTTP server. Instead, Joern can be fully embedded in your application.

For code property graph creation, Joern includes
[fuzzyc2cpg](https://github.com/ShiftLeftSecurity/fuzzyc2cpg), a fuzzy
C/C++ language module. The following Scala test code demonstrates how FuzzyC2CPG can be used to create a code property graph from all C/C++ files in the directory `joern-cli/src/test/resources/testcode/free` and save it at `/tmp/cpg.bin.zip`.

{{<snippet file="src/test/scala/io/shiftleft/joern/GenerationTests.scala" language="scala">}}

For your convenience, Joern includes two thin test programs around this API, `joern-parse` and `joern-query`, which allow creating and querying CPGs respectively.

## Creating Code Property Graphs with `joern-parse`

```
./joern-parse <path/to/directory> --out <path/to/cpg/cpg_name>
```

If you ommit the ```--out``` flag, the CPG is named `cpg.bin.zip` and stored in the local folder.

As an example, run
```
./joern-parse tests/free
```
to create a CPG for the test project `free`.

To view all options offered by `fuzzyc2cpg`, simply run
```
./joern-parse
```

## Querying Code Property Graphs with `joern-query`

One you have created a code property graph, you can load and query it with `joern-query`:

```
./joern-query <query>
```

For example, you can run
```
./joern-query cpg.method.name
```

to obtain the names of all methods. By default, the code property
graph at `cpg.bin.zip` is loaded. You can specify an alternative CPG
using the `-c` flag. For example,

```
./joern-query -c path/to/my/cpg.bin.zip cpg.method.name
```
will process the CPG at `path/to/my/cpg.bin.zip`.

## Passing scripts to `joern-query`

Instead of specifying a query on the shell, you can also pass scripts
to `joern-query` as follows:

```
./joern-query -f myscript.scala
```

To list all methods via a script, try the following:

```
echo "cpg.method.name.l.mkString(\"\\n\")" > foo.scala
./joern-query -f foo.scala 
```
