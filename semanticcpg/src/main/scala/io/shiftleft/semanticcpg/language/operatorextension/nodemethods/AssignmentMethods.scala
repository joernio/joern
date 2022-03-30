package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class AssignmentMethods(val assignment: OpNodes.Assignment) extends AnyVal {

  def target: Expression = assignment.argument(1)

  def source: Expression = {
    val numberOfArguments = assignment.argument.size
    numberOfArguments match {
      case 1 => assignment.argument(1)
      case 2 => assignment.argument(2)
      case _ => throw new RuntimeException(s"Assignment statement with $numberOfArguments arguments")
    }
  }
}
