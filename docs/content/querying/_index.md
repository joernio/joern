+++
Title="Querying Code Property Graphs"
weight=5
alwaysopen=true
+++

Joern/Ocular provides a domain specific language (DSL) for code
analysis based on the Scala programming language. The great advantage
of this approach is that all features of the host language are
automatically inherited. Joern queries are simply lazily evaluated
Scala expressions, and you can make use of the complete Scala
programming language in Joern scripts.


# Basic Concepts: Expressions, Labels, IDs

To get a first idea of what information the Code Property Graph makes
available, start the `joern` shell and type `cpg.`, then press the Tab
key:


```bash
joern> cpg.
all             file            local           methodReturn    types
argument        graph           member          namespace
call            id              metaData        namespaceBlock
close           identifier      method          parameter
comment         literal         methodRef       typeDecl

```

The following query returns *all* nodes upon evaluation:

```bash
cpg.all
res8: NodeSteps[StoredNode] = io.shiftleft.semanticcpg.language.NodeSteps@4cce421e
```
Notice that `cpg.all` is an **expression**, and this is expression is
only **evaluated** if so requested, e.g., by converting the expression to a
list as in the following query.

```bash
cpg.all.toList.size
711
```

This query returns the number of nodes in the graph, that is, the size
of the list obtained by evaluating the expression `cpg.all`. Since
converting to lists is such a common operation, we have introduced the
shorthand `.l` for `.toList`. The previous query can thus also be
written as:

```bash
cpg.all.l.size
711
```

Each node has a **label** and a unique **id**. The label indicates the
type of program construct represented by the node. To get an overview
of labels in the graph, you can issue the following query:

```bash
cpg.all.label.toSet.toList.sorted
res5: List[String] = List(
  "BLOCK",
  "CALL",
  "COMMENT",
  "CONTROL_STRUCTURE",
  "FILE",
  "IDENTIFIER",
  "LITERAL",
  "LOCAL",
  "META_DATA",
  "METHOD",
  "METHOD_INST",
  "METHOD_PARAMETER_IN",
  "METHOD_PARAMETER_OUT",
  "METHOD_RETURN",
  "NAMESPACE",
  "NAMESPACE_BLOCK",
  "RETURN",
  "TYPE",
  "TYPE_DECL",
  "UNKNOWN"
)
```

In this query, the expression `cpg.all.label` is evaluated as the
method `toSet` is called. The resulting Scala set is converted to a
list and sorted. Note that both `toList` and `sorted` are API methods
of the host language Scala.

The **id** is a long integer that uniquely identifies the node. It can
be used to jump to the node in subsequent queries. For example, you
can determine the ids of all File nodes as follows:

```bash
cpg.file.id.l
res11: List[AnyRef] = List(34L, 32L, 30L, 28L, 26L, 24L, 22L, 20L, 18L, 16L, 14L, 12L, 10L, 8L, 6L, 4L)
```

To select the node with the id `10`, you can use the following query:

```bash
cpg.id(10).l
res7: List[Nothing] = List(
  File(id -> 10L, name -> "/home/tmp/shiftleft/joern/tarpit-c/tarpitc/buffer_underwrite.c", order -> null)
)
```

Note that the type of the nodes returned by `cpg.id(10)` is unknown at
compile time, and therefore, the returned type for `cpg.id(10).l`is
`List[Nothing]`. In practice, you will know the types of nodes you are
expecting from the previous query. For example, in the outlined
scenario, we know that case are file nodes. An explicit cast can be
performed to bring us back into the typed world:

```bash
cpg.id(10).asInstanceOf[NodeSteps[nodes.File]].name.l
res24: List[String] = List("/home/tmp/shiftleft/joern/tarpit-c/tarpitc/buffer_underwrite.c")

```


## Query Structure

Queries can be broken down into three the following three parts.

* **Start node selection.** All traversals begin by selection of a set
  of start nodes, e.g., all methods of the graph, all locals, or all
  parameters with a specified type. This is where we begin walking the
  graph.


* **Walking the graph.** Starting at the nodes selected in the
  previous part, the query walks along the graph edges to reach
  adjacent nodes according to properties and types of nodes and
  edges. The final goal of the traversal is to determine all nodes
  that can be reached by the traversal. This part of the query ends
  with evaluation, e.g., via `.l` or `.toSet`.

* **Result preparation.** Finally, results are prepared such that they
  can be easily interpreted or processed by subsequent tools. This
  step may involve triggering auxiliary queries.



Let's illustrate this by example. Consider the following query:

```bash
cpg.parameter.evalType(".*char.*")
   .method.name(".*parse.*")
   .map(x => (x.name, x.start.caller.id.l))
```

This query describes which nodes to select
(`cpg.parameter.evalType(".*char.*")` - all parameters with evaluation
type `.*char.*`), how to walk the graph (`.method.name(".*parse.*")` -
see if it is possible to walk to the associated method and find that
its name matches `.*parse.*`), and finally, how to prepare results
(`.map(x => (x.name, x.start.caller.id.l))` - return (name,caller)
pairs, triggering the auxiliary query `x.start.caller.id.l` to obtain
callers).


## DocuTests

Throughout the documentation of the query language, we make use of
DocuTests. A DocuTest is a small code snippet that defines sample
code, queries and expected outcomes. As an example, the snippet below
shows a DocuTest that demonstrates the behavior the
`cpg.method.name.toSet` on a sample snippet containing only an empty
main function.

{{<snippet file="codepropertygraph/semanticcpg/src/test/scala/io/shiftleft/semanticcpg/language/SampleDocuTest.scala" language="scala">}}

The test contains a definition of the sample code (`code`), and the
corresponding code property graph in `cpg`. It then specifies that
`cpg.method.name.toSet` should return the set `Set("main")`, that is,
"main" is the only method in the graph.
