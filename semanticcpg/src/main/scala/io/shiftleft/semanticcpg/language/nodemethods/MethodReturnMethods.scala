package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodReturn, NewLocation, Type}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class MethodReturnMethods(val node: MethodReturn) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(node, "$ret", node.label, node.lineNumber, node.method)
  }

  def returnUser(implicit callResolver: ICallResolver): Iterator[Call] = {
    val method    = node._methodViaAstIn
    val callsites = callResolver.getMethodCallsites(method)
    // TODO for now we filter away all implicit calls because a change of the
    // return type to CallRepr would lead to a break in the API aka
    // the DSL steps which are subsequently allowed to be called. Before
    // we addressed this we can only return Call instances.
    callsites.collectAll[Call]
  }

  // TODO define in schema as named step
  def typ: Iterator[Type] = node.evalTypeOut
}
