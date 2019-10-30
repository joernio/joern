+++
Title="Query Language"
weight=5
alwaysopen=true
+++

Joern/Ocular provides a domain specific language (DSL) for code
analysis based on [Scala](https://www.scala-lang.org/). It is an
internal DSL, that is, it is a language defined via classes and
methods as opposed to a formal grammar. The great advantage of this
approach is that all features of the host language are automatically
inherited. Joern queries are lazily evaluated Scala expressions, and
you can make use of the complete Scala programming language in Joern
scripts. In this section, we look into the basic concepts of our query
language and illustrate them by example.

In the following examples, we assume that a code property graph for
the *tarpitc* program is loaded. For details on creating and loading
CPGs, please check out [Importing Code](../importing) and [The Joern
Shell](../shell).



# Nodes, Labels, and Expressions

To begin exploring the data that the language exposes,
let's create a query that selects all nodes in the code property graph with
`cpg.all`:

```bash
cpg.all
res8: NodeSteps[StoredNode] = io.shiftleft.semanticcpg.language.NodeSteps@4cce421e
```

Notice that the result is not a list of nodes but rather a variable type
`NodeSteps[StoredNode]`. You can think of this as an *expression*
which - only upon evaluation - yields all nodes [1]. The advantage of
this two stage approach is that we can already perform calculations on
expressions prior to costly evaluation. In particular, we can use the
type system to perform validity checks and query optimization,
features that are vital as we compose expressions to form complex
queries.


To evaluate the expression, we can simply force conversion to a list:

```bash
cpg.all.toList.size
711
```

This query returns the number of nodes in the graph, that is, the size
of the list obtained containing all nodes. Since converting to lists
is such a common operation, we have introduced the shorthand `.l` for
`.toList`. The previous query can thus also be written as:

```bash
cpg.all.l.size
711
```

Each node has a **label**. The label indicates the type of program
construct represented by the node. To get an overview of labels in the
graph, you can issue the following query:

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

[1] The term `Step` is inherited from the graph traversal language
[Gremlin](http://tinkerpop.apache.org/docs/current/reference/#graph-traversal-steps),
and more specifically
[gremlin-scala](https://github.com/mpollmeier/gremlin-scala) on which
our DSL is based. In Gremlin, `Steps` describe walks in a graph, which
are combined via function composition to yield *traversals*. You can
think of traversals as expressions that describe sets of nodes.



## Expression Types and Starters

For each node label, a corresponding *expression type* is defined in the
query language, e.g., `Method` for nodes with the label `METHOD`, and
`Local` for nodes with the label `LOCAL`. For each label, we also
define a *starter* that represents all nodes with that label. For
example,

```bash
cpg.method
res1: types.structure.Method = io.shiftleft.semanticcpg.language.types.structure.Method@b889cb6
```
returns an expression that yields all methods on evaluation.

You can get a complete list of starters by typing `cpg.`, and
pressing the Tab key:

```bash
joern> cpg.
all             file            local           methodReturn    types
argument        graph           member          namespace
call            id              metaData        namespaceBlock
close           identifier      method          parameter
comment         literal         methodRef       typeDecl

```

It is generally advisable to use specific starters as opposed to the
generic `cpg.all`, as for the latter, it is unclear at compile time
which types of nodes are processed. Using specific starters such as
`.method` make it possible to use the Scala's type system for
completion and to detect invalid queries even before evaluation.

## Node IDs

For each node, we store an **ID**. The ID is a long integer that
uniquely identifies the node.You should not need to ever retrieve
node IDs within Joern/Ocular scripts, however, they may come in handy
when passing nodes to external tools - especially if these tools plan
to run subsequent queries.

As an example, the following query returns the ids of all file nodes.

```bash
cpg.file.id.l
res11: List[AnyRef] = List(34L, 32L, 30L, 28L, 26L, 24L, 22L, 20L, 18L, 16L, 14L, 12L, 10L, 8L, 6L, 4L)
```

Consider that - upon receiving this result, an external tool would
like to invoke a follow-up query on the first file node. To achieve
this, it can issue the query

```bash
cpg.id(34).l
res7: List[Nothing] = List(
  File(id -> 10L, name -> "/home/tmp/shiftleft/joern/tarpit-c/tarpitc/buffer_underwrite.c", order -> null)
)
```

Note that the type of the nodes returned by `cpg.id(34)` is unknown at
compile time, and therefore, the returned type for `cpg.id(34).l`is
`List[Nothing]`. In practice, the external tool will usually know the
node type from the previous query, e.g., in our example, we know that
the node with the ID `34` is a file node. An explicit cast can be
performed to bring us back into the typed world:

```bash
cpg.id(34).asInstanceOf[NodeSteps[nodes.File]].name.l
res24: List[String] = List("/home/tmp/shiftleft/joern/tarpit-c/tarpitc/buffer_underwrite.c")

```

Finally, `cpg.id` also accepts a sequence of nodes. For example

```bash
cpg.id(Seq(34,10))
```

returns the nodes with IDs `34` and `10`.


## Overall Query Structure

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
