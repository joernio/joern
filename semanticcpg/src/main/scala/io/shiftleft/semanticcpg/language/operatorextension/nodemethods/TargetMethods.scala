package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.{OpNodes, allArrayAccessTypes}

class TargetMethods(val expr: Expression) extends AnyVal {

  def arrayAccess: Option[OpNodes.ArrayAccess] =
    expr.ast.isCall
      .cast[OpNodes.ArrayAccess]
      .collectFirst { case x if allArrayAccessTypes.contains(x.name) => x }

  def pointer: Option[Expression] =
    Option(expr).collect {
      case call: Call if call.name == Operators.indirection => call.argument(1)
    }

}
