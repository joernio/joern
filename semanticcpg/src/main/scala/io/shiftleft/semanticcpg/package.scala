package io.shiftleft

import overflowdb.traversal.help.Table.AvailableWidthProvider

/** Domain specific language for querying code property graphs
  *
  * This is the API reference for the CPG query language, a language to mine code for defects and vulnerabilities both
  * interactively on a code analysis shell (REPL), or using non-interactive scripts.
  *
  * Queries written in the CPG query language express graph traversals (see
  * [[https://en.wikipedia.org/wiki/Graph_traversal]]). Similar to the standard graph traversal language "Gremlin" (see
  * [[https://en.wikipedia.org/wiki/Gremlin_(programming_language)]])) these traversals are formulated as sequences of
  * primitive language elements referred to as "steps". You can think of a step as a small program, similar to a unix
  * shell utility, however, instead of processing lines one by one, the step processes nodes of the graph.
  *
  * ==Starting a traversal==
  * All traversals begin by selecting a set of start nodes, e.g.,
  *
  * {{{cpg.method}}}
  *
  * will start the traversal at all methods, while
  *
  * {{{cpg.local}}}
  *
  * will start at all local variables. The complete list of starting points can be found at
  * {{{io.shiftleft.codepropertygraph.generated.Cpg}}}
  *
  * ==Lazy evaluation==
  * Queries are lazily evaluated, e.g., `cpg.method` creates a traversal which you can add more steps to. You can, for
  * example, evaluate the traversal by converting it to a list:
  *
  * {{{cpg.method.toList}}}
  *
  * Since `toList` is such a common operation, we provide the shorthand `l`, meaning that
  *
  * {{{cpg.method.l}}}
  *
  * provides the same result as the former query.
  *
  * ==Properties==
  * Nodes have "properties", key-value pairs where keys are strings and values are primitive data types such as strings,
  * integers, or Booleans. Properties of nodes can be selected based on their key, e.g.,
  *
  * {{{cpg.method.name}}}
  *
  * traverses to all method names. Nodes can also be filtered based on properties, e.g.,
  *
  * {{{cpg.method.name(".*exec.*")}}}
  *
  * traverse to all methods where `name` matches the regular expression ".*exec.*". You can see a complete list of
  * properties by browsing to the API documentation of the corresponding step. For example, you can find the properties
  * of method nodes at [[io.shiftleft.semanticcpg.language.types.structure.MethodTraversal]].
  *
  * ==Side effects==
  * Useful if you want to mutate something outside the traversal, or simply debug it: This prints all typeDecl names as
  * it traverses the graph and increments `i` for each one.
  * {{{
  *   var i = 0
  *   cpg.typeDecl.sideEffect{typeTemplate => println(typeTemplate.name); i = i + 1}.exec
  * }}}
  *
  * ==[advanced] Selecting multiple things from your traversal==
  * If you are interested in multiple things along the way of your traversal, you label anything using the `as`
  * modulator, and use `select` at the end. Note that the compiler automatically derived the correct return type as a
  * tuple of the labelled steps, in this case with two elements.
  *
  * {{{
  *   cpg.method.as("method").definingTypeDecl.as("classDef").select.toList
  *   // return type: List[(Method, TypeDecl)]
  * }}}
  *
  * ==[advanced] For comprehensions==
  * You can always start a new traversal from a node, e.g.,
  * {{{
  *   val someMethod = cpg.method.head
  *   someMethod.start.parameter.toList
  * }}}
  *
  * You can use this e.g. in a for comprehension, which is (in this context) essentially an alternative way to select
  * multiple intermediate things. It is more expressive, but more computationally expensive.
  * {{{
  *   val query = for {
  *     method <- cpg.method
  *     param <- method.start.parameter
  *   } yield (method.name, param.name)
  *
  *   query.toList
  * }}}
  */

package object semanticcpg {

  implicit val defaultAvailableWidthProvider: AvailableWidthProvider =
    () => replpp.util.terminalWidth.filter(_ > 0).getOrElse(120)

}
