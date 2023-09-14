package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.v2.nodes.Local

object OpNodes {

  /** Represents a module-level global variable. This kind of node behaves like both a local variable and a field access
    * and is common in languages such as Python/JavaScript.
    */
  class ModuleVariable(node: Local) extends Local(node.graph, node.seq())

}
