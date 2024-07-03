package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.language.*

/** A local variable
  */
class LocalTraversal(val traversal: Iterator[Local]) extends AnyVal {

  /** The method hosting this local variable
    */
  def method: Iterator[Method] = {
    // TODO The following line of code is here for backwards compatibility.
    // Use the lower commented out line once not required anymore.
    traversal.repeat(_.in(EdgeTypes.AST))(_.until(_.hasLabel(NodeTypes.METHOD))).cast[Method]
    // definingBlock.method
  }

}
