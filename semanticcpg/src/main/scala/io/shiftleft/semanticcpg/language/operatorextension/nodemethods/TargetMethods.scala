package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.{OpNodes, allArrayAccessTypes}
import overflowdb.traversal.Traversal

class TargetMethods(val expr: Expression) extends AnyVal {

  def arrayAccess: Option[OpNodes.ArrayAccess] =
    expr.ast.isCall
      .collectFirst { case x if allArrayAccessTypes.contains(x.name) => x }
      .map(new OpNodes.ArrayAccess(_))

  def pointer: Traversal[Expression] = {
    expr match {
      case call: Call if call.name == Operators.indirection => call.argument(1)
      case _                                                => Traversal()
    }
  }

}
