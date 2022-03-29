package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._
import overflowdb.traversal.help.Doc

@help.Traversal(elementType = classOf[MethodReturn])
class MethodReturnTraversal(val traversal: Traversal[MethodReturn]) extends AnyVal {

  @Doc(info = "traverse to parent method")
  def method: Traversal[Method] =
    traversal.in(EdgeTypes.AST).cast[Method]

  def returnUser(implicit callResolver: ICallResolver): Traversal[Call] =
    traversal.flatMap(_.returnUser)

  /** Traverse to last expressions in CFG. Can be multiple.
    */
  @Doc(info = "traverse to last expressions in CFG (can be multiple)")
  def cfgLast: Traversal[CfgNode] =
    traversal.in(EdgeTypes.CFG).cast[Expression]

  /** Traverse to return type
    */
  @Doc(info = "traverse to return type")
  def typ: Traversal[Type] =
    traversal.flatMap(_.evalTypeOut)
}
