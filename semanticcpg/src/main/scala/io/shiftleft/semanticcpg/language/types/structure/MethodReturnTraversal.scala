package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help
import overflowdb.traversal.help.Doc

@help.Traversal(elementType = classOf[MethodReturn])
class MethodReturnTraversal(val traversal: Iterator[MethodReturn]) extends AnyVal {

  @Doc(info = "traverse to parent method")
  def method: Iterator[Method] =
    traversal.flatMap(_._methodViaAstIn)

  def returnUser(implicit callResolver: ICallResolver): Iterator[Call] =
    traversal.flatMap(_.returnUser)

  /** Traverse to last expressions in CFG. Can be multiple.
    */
  @Doc(info = "traverse to last expressions in CFG (can be multiple)")
  def cfgLast: Iterator[CfgNode] =
    traversal.flatMap(_.cfgIn)

  /** Traverse to return type
    */
  @Doc(info = "traverse to return type")
  def typ: Iterator[Type] =
    traversal.flatMap(_.evalTypeOut)
}
