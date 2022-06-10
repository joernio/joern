package io.joern.javasrc2cpg.util

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall

// TODO: Decide which of these should be moved to x2cpg
object NodeBuilders {
  def assignmentNode(): NewCall = NewCall()
    .name(Operators.assignment)
    .methodFullName(Operators.assignment)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)

  def indexAccessNode(): NewCall = NewCall()
    .name(Operators.indexAccess)
    .methodFullName(Operators.indexAccess)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
}
