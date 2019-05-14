+++
Title="Base Language"
weight=2
alwaysopen=true
+++

In this section, we present the base query language via *DocuTests*. A
DocuTest is a small code snippet that defines sample code, queries and
expected outcomes. As an example, the snippet below shows a DocuTest
that demonstrates the behavior the `cpg.method.name.l` on a sample
snippet containing only an empty main function.

{{<snippet file="src/test/scala/io/shiftleft/joern/SampleDocuTest.scala" language="scala">}}

The test contains a definition of the sample code as a first argument
to the constructor `TestCpg`, and the corresponding code property
graph in `cpg`. It then specifies that `cpg.method.name.toSet` should
return the set `Set("main")`, that is, "main" is the only method in
the graph.
