package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.nodes.{Block, Member}

object OpNodes {

  /** Represents a module-level global variable. This kind of node behaves like both a local variable and a field access
    * and is common in languages such as Python/JavaScript.
    */
  class ModuleVariable(node: Member) extends Member(node.graph(), node.id)

  /** Represents a module variable access block, which represents a lowered view of operations taken on both the local
    * variable and member-level access.
    *
    * @see
    *   <a href="https://github.com/joernio/joern/pull/3750/">[pysrc2cpg] Model Field-like Behaviour of Module
    *   Variables</a>
    */
  class ModuleVariableReference(node: Block) extends Block(node.graph(), node.id)

}
