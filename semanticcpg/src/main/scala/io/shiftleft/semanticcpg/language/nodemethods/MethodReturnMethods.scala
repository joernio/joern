package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodReturn, NewLocation, Type}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.{HasLocation, LocationCreator, _}
import overflowdb.traversal.Traversal

class MethodReturnMethods(val node: MethodReturn) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(node, "$ret", node.label, node.lineNumber, node.method)
  }

  def returnUser(implicit callResolver: ICallResolver): Traversal[Call] = {
    val method    = node._methodViaAstIn
    val callsites = callResolver.getMethodCallsites(method)
    // TODO for now we filter away all implicit calls because a change of the
    // return type to CallRepr would lead to a break in the API aka
    // the DSL steps which are subsequently allowed to be called. Before
    // we addressed this we can only return Call instances.
    Traversal.from(callsites.filter(_.isInstanceOf[Call])).cast[Call]
  }

  def typ: Traversal[Type] = node.evalTypeOut
}
