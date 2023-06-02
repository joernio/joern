# The great Traversal removal (v1.2.0; June 2023)
PR: https://github.com/joernio/joern/pull/2784
## Summary
We removed the `overflowdb.traversal.Traversal` class, and replaced it with a type alias `type Traversal[+T] = scala.collection.Iterator[T]`.
In tandem, we improved consistency between the types of quasi-Iterators that appear in common APIs. 

This improved consistency comes at a price -- this change is not backwards compatible. The needed adaptations are typically very superficial,
often simply a change in imports. On the upside, this change is extremely unlikely to introduce bugs / semantic changes. Instead, it will cause
compile errors. Unfortunately the compiler error messages are not always helpful in fixing the issue, so we have a handy migration guide.

## Old model
In the old world, our APIs returned a mix of `scala.collection.Iterator`, `java.util.Iterator` and `overflowdb.traversal.Traversal`. These three
types are semantically equivalent -- they all are lazy with respect to operations like `.map` and side-effects, they are stateful, i.e. are 
mutated in-place when advancing, and are not reusable.

The mismatch between these classes is partially hidden from the user by extensive use of implicit conversions. These are implemented by wrapping;
hence we often end up with a Traversal that wraps a scala Iterator that wraps a java Iterator that finally contains the actual iteration state.

In addition to the directly provided features by `Traversal`, like the pervasively used shorthand `.l` for `.toList`, or complex query-language
features like path-tracking or logic like `.where` or even graph-search (breadth-first or depth-first) with `.repeat`, `Traversal` mixes in scala's
`IterableOps`. This provides pervasively used methods like `.head` alongside not-so-pervasively used methods like `.view` -- the latter being an 
example that silently misbehaves on `Traversal`, since the mixin `IterableOps` is designed for stateless collections that can be traversed multiple times.

## New model
`Traversal` is replaced by scala `Iterator`. Most functionality from `Traversal` is implemented as extension methods to `IterableOnce` (which is a
superclass of `Iterator`) that yield `Iterator`. Many old APIs that used to return java iterators now return scala iterators.

## Migration guide
The following is a list of examples of common fixes that were needed to adapt the joern codebase to this change, together with an explanation and
the compiler errors. Downstream users of joern likely have similar usage patterns, so this should help with the migration. 

You can see the entire diff [here](https://github.com/joernio/joern/pull/2784/files)

### Superfluous `.asScala`
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/dotgenerator/CdgGenerator.scala:14:15: value asScala is not a member of Iterator[io.shiftleft.codepropertygraph.generated.nodes.StoredNode]
[error]     v._cdgOut.asScala
```
The offending code was
```
  override def expand(v: StoredNode): Iterator[Edge] = {
    v._cdgOut.asScala
      .filter(_.isInstanceOf[StoredNode])
      .map(node => Edge(v, node, edgeType = edgeType))
  }
```
The `.asScala` here used to create a scala Iterator that wraps the underlying java iterator that used to be returned by `_cdgOut`.

We have changed the API such that `_cdgOut` now directly returns a scala iterator, no wrapping required. Hence the fix is to remove
the superfluous `.asScala` call:
```
  override def expand(v: StoredNode): Iterator[Edge] = {
    v._cdgOut
      .filter(_.isInstanceOf[StoredNode])
      .map(node => Edge(v, node, edgeType = edgeType))
  }
```
This issue affected 10 files in joern.

### Missing `.asScala`
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/NodeTypeStarters.scala:18:21: value cast is not a member of java.util.Iterator[overflowdb.Node]
[error] did you mean wait?
[error]     cpg.graph.nodes.cast[StoredNode]
```
Some legacy API methods still return java iterators. We had an implicit conversion from java iterator to `Traversal` which was triggered in this case --
java iterators have no member function named `cast`, and there is a unique conversion to a type that has such a member function.

Now `cast` exists on an extension (`AnyVal`) class, and there is an implicit conversion from scala `IterableOnce` to this extension. This implicit conversion
does not exist on java iterators; hence it is necessary to manually convert, i.e.
```
import scala.jdk.CollectionConverters.IteratorHasAsScala
...
    cpg.graph.nodes.asScala.cast[StoredNode]

```
This issue affected 4 files in joern.

Technically speaking, this category can be considered a sub-class of [Reliance on implicit conversion to Traversal](#reliance-on-implicit-conversion-to-traversal).

### Access to the `Traversal` companion object
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/dotgenerator/DotCallGraphGenerator.scala:10:5: not found: value Traversal
[error]     Traversal(DotSerializer.dotGraph(None, callGraph))
```
The offending code was
```
  def dotCallGraph(cpg: Cpg): Traversal[String] = {
    val callGraph = new CallGraphGenerator().generate(cpg)
    Traversal(DotSerializer.dotGraph(None, callGraph))
  }
```
This snippet used to call `Traversal:apply` on the `Traversal` companion object as a factory function to produce a `Traversal` over a 
single element. The return type of `dotCallGraph` does not require adjustment due to the type-alias that replaces the old Traversal class.
Unfortunately the type-alias cannot redirect access to the old companion object (which does not exist anymore). Hence the fix is to use the
`Iterator` companion object instead:
```
  def dotCallGraph(cpg: Cpg): Traversal[String] = {
    val callGraph = new CallGraphGenerator().generate(cpg)
    Iterator(DotSerializer.dotGraph(None, callGraph))
  }
```
NB: In more performance-sensitive parts of the codebase, it is advisable to use the `Iterator.single` factory method instead of the
`Iterator.apply` factory method, due to implementation details of the scala standard library: `Iterator.apply` is vararg and hence 
introduces another layer of indirection via a one-element argument array. It is unclear why the implementors decided against a dedicated
overload for the common case of one-element iterators.

Almost the same issue can occur, and is fixed similarly, with `PathAwareTraversal`, yielding errors like
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/callgraphextension/MethodTraversal.scala:18:7: not found: value PathAwareTraversal
[error]       PathAwareTraversal.empty
```
This issue affected 36 files in joern.

### Reliance on implicit conversion to Traversal
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/TagTraversal.scala:22:59: type mismatch;
[error]  found   : Seq[A]
[error]  required: io.shiftleft.semanticcpg.language.Traversal[A]
[error]     (which expands to)  Iterator[A]
[error]     traversal.in(EdgeTypes.TAGGED_BY).collectAll[A].sortBy(_.id)
```
The offending code was
```
  private def tagged[A <: StoredNode: ClassTag]: Traversal[A] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[A].sortBy(_.id)
```
The `sortBy` method on Traversal returns a `Seq`. This code used to work because there was an implicit conversion to `Traversal`.

In this specific case, we need to make the conversion explicit, by using
```
private def tagged[A <: StoredNode: ClassTag]: Traversal[A] =
    traversal.in(EdgeTypes.TAGGED_BY).collectAll[A].sortBy(_.id).iterator
```

NB. It is advantageous to improve this line further, using the (domain specific) CPG API:
```
private def tagged[A <: StoredNode: ClassTag]: Traversal[A] =
    traversal._taggedByIn.collectAll[A].sortBy(_.id).iterator
```

Another example happened in the following code:
```
val method           = methodReturn.method.head
```
Here, `methodReturn` was a single `MethodReturn` node, and `method` was a single `Method` node. The author of the offending snippet
was evidently confused about the type and expected to have a `Traversal[Method]` in hand that contains the single required `Method`.
This code used to compile, due to an implicit conversion from a single node to `Traversal[Node]`, and due to `Traversal[Node]` having the
member function `head`, mixed in from `IterableOps`.

Such code does not compile anymore. The solution is to remove the superfluous `.head` call, i.e.
```
val method           = methodReturn.method
```

Another example happened in the following code:
```
  def globalFromLiteral(lit: Literal): Traversal[Expression] = lit
    .where(_.inAssignment.method.nameExact("<module>", ":package"))
    .inAssignment
    .argument(1)
```
This code used to work due to the same implicit conversion, `where` being a member method of `Traversal`. This now needs to be explicit, via either
```
  def globalFromLiteral(lit: Literal): Traversal[Expression] = Iterator(lit)
    .where(_.inAssignment.method.nameExact("<module>", ":package"))
    .inAssignment
    .argument(1)
```
or the fluent
```
  def globalFromLiteral(lit: Literal): Traversal[Expression] = lit.start
    .where(_.inAssignment.method.nameExact("<module>", ":package"))
    .inAssignment
    .argument(1)
```

Another example happened in the following snippets:
```
  implicit def toDdgNodeDot(traversal: IterableOnce[Method]): DdgNodeDot =
    new DdgNodeDot(traversal)
//...
class DdgNodeDot(val traversal: Traversal[Method]) extends AnyVal {
//...
```
This code used to work due to an implicit conversion from `IterableOnce` to `Traversal`. We do not have this implicit conversion anymore, and the code should instead read:
```
  implicit def toDdgNodeDot(traversal: IterableOnce[Method]): DdgNodeDot =
    new DdgNodeDot(traversal.iterator)
```
This is an important design pattern in our extensions methods: We accept `IterableOnce` for our extensions, in order to avoid users
having to type `.iterator` everywhere in their code, and we always produce `Iterator`, in order to make it easy to reason about the semantics.

This issue affected 27 files in joern.

### Use of the `count` step
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/NodeSteps.scala:90:54: missing argument list for method count in trait IterableOnceOps
[error] Unapplied methods are only converted to functions when a function type is expected.
[error] You can make this conversion explicit by writing `count _` or `count(_)` instead of `count`.
[error]       .repeat(_.in(edgeType))(_.until(_.in(edgeType).count.filter(_ == 0)))
```
There used to be a step `count: Traversal[Int]` that turns a traversal into a new traversal with a single integer element that contains the number of items in
the original traversal.

Unfortunately there is a name clash with an existing `count` method on `IterableOnceOps`. The old code had no problem with that, since a direct 
member function shadows the mixin. Since we now use extension methods on vanilla iterators, we cannot shadow the mixin. We therefore renamed 
the traversal step to `countTrav`, and the code should now read
```
      .repeat(_.in(edgeType))(_.until(_.in(edgeType).countTrav.filter(_ == 0)))
```
This issue affected 1 file in joern.

### Double import of Traversal and implicits
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/nodemethods/StoredNodeMethods.scala:11:12: reference to Traversal is ambiguous;
[error] it is imported twice in the same scope by
[error] import overflowdb.traversal._
[error] and import io.shiftleft.semanticcpg.language._
[error]   def tag: Traversal[Tag] = {
```
The import `import io.shiftleft.semanticcpg.language._` includes almost all things that are imported by `import overflowdb.traversal._`.
These two imports were always conflicting, in the sense that the overflowdb import was redundant and stopped many things
from working due to ambiguity problems. Now, many more things stop working with such broken imports.

The fix is to remove the offending `import overflowdb.traversal._`.

This issue affected 37 files in joern.

### Missing import of traversal extensions
```
[error] joern/semanticcpg/src/main/scala/io/shiftleft/semanticcpg/language/types/structure/AnnotationParameterAssignTraversal.scala:21:8: value cast is not a member of Iterator[io.shiftleft.codepropertygraph.generated.nodes.AstNode]
[error] did you mean wait?
[error] possible cause: maybe a semicolon is missing before `value cast`?
[error]       .cast[Expression]
```
Some operations on Traversal where implemented as member methods, as opposed to extension methods. These operations used to work without any imports of implicit conversions / extensions.
These operations now need `import io.shiftleft.semanticcpg.language._` in order to work.

This issue affected 7 files in joern.

### Stylistic Changes

There are some stylistic changes in which APIs are used that we wish to encourage in the userbase.

Historically speaking, the first versions of joern used a more "property graph database"-style model.

Such a model suggests writing parametrized queries in a domain-specific language, comparable to e.g. SQL. Then the parametrized query is sent to the database server, parsed, checked against the database schema, optimized, and cached as a prepared query. Later uses refer to the prepared query, and fill in the details. Since the query preparation is amortized over all its calls, and since any database access over any kind of network is always very expensive (measured in milliseconds or microseconds, as opposed to nanoseconds), there is no great pressure to internalize the schema.

Instead of using some raw DSL, it is common to provide a fluent query-builder API. This is how much joern code looks like:
```
 def topLevelExpressions: Traversal[Expression] =
     traversal
      .out(EdgeTypes.AST)
      .hasLabel(NodeTypes.BLOCK)
      .out(EdgeTypes.AST)
      .not(_.hasLabel(NodeTypes.LOCAL))
```

However, this was never how joern actually worked: As we can see in the above snippet, `topLevelExpressions` is defined as a function, not a `lazy val`. In other words, function calls like `.out`, and all the string handling, were never in practice amortized over multiple uses of the expression; nobody ever wrote a query optimizer / compiler; and it was never possible to reasonably use joern with a remote database, due to the involved latencies (joern code expects access latencies counted in nanoseconds, not milliseconds).

Early versions of joern used TinkerGraph as a graph database. TinkerGraph internally represents nodes as individual hashtables. As such, the above was actually the "close to the metal" representation of how to e.g. follow the out-edges of type "AST".

This is incredibly wasteful in terms of memory: The relevant keys are known at compile time. Hence, overflowdb uses generated domain-specific classes, together with functions to access the relevant fields, like e.g.
```
 def topLevelExpressions: Traversal[Expression] =
     traversal
      ._astOut
      .collectAll[Block]
      ._astOut
      .not(_._collectAll[Local])
```
The old APIs still exist for compat reasons. However, using a string to select a property, node-type, or edge-label is akin to using java reflection to access a field: It is to be done only when in great need, and is definitely a code-smell. In most cases, there are better ways available.

There is a common pattern in java, if one must use reflection: One reflectively constructs a VarHandle or MethodHandle, and stores it in a static (i.e. effectively global) variable. Hence, the reflection is done at class-loading time, not at actual runtime. The JVM is actually quite adept at making such use of reflection fast. (note the parallel to prepared queries in databases!)

The way to hoist the overhead from string-based lookups in joern is to add the required functions in overflowdb-codegen (like e.g. the `_astOut` instead of `.out("AST")`).

This issue affects many files in joern; https://github.com/joernio/joern/pull/2784 includes fixes for 7 of them.
