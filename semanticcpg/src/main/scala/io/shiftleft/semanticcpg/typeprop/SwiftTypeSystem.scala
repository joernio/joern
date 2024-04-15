package io.shiftleft.semanticcpg.typeprop
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.typeprop.TypeProp2.SymFn

import scala.collection.mutable

class SwiftTypeSystem() extends TypeSystem[nodes.StoredNode] {

  override def inputsOff(output: nodes.StoredNode): InputsAndTransferFunc[nodes.StoredNode] = {
    output match {
      case call: nodes.Call if call.methodFullName == Operators.assignment =>
        
      case call: nodes.Call if call.dispatchType == DispatchTypes.STATIC_DISPATCH =>
        // TODO for now we ignore receiver and argument
        val receiverAndArguments = call.receiver ++ call.argument
        val transferFunc = () => {
          Some(call.methodFullName)
        }
        new InputsAndTransferFunc(receiverAndArguments.toBuffer, transferFunc)
      case identifier: nodes.Identifier =>
        val local = identifier.refsTo.take(1)
        new InputsAndTransferFunc(local.toBuffer, handleIdentifier)
      case local: nodes.Local =>
        // TODO use DFO here. We need the initialising assignment to a local variable
        val firstIdentifier = local.referencingIdentifiers.sortBy(_.lineNumber).head
        val parentCall = firstIdentifier.astParent.asInstanceOf[nodes.Call]
        val transferFunc = () => {
          val parentCallSymbol = pointToFullName.get(parentCall).flatMap(fullNameToSym.get)
          val paramFullName = parentCallSymbol.get.paramFullNames(firstIdentifier.argumentIndex)
          Some(paramFullName)
        }
        new InputsAndTransferFunc(parentCall.toBuffer, transferFunc)
    }
  }

  private def handleCall(methodFullName: String)(inputs: collection.Seq[Option[String]]): Option[String] = {
    val receiver = inputs.head
    val arguments = inputs.tail

    symbolProvider.getSymbol(methodFullName).map(_.fullName)
  }

  private def handleIdentifier(inputs: collection.Seq[Option[String]]): Option[String] = {
    val localSymFullName = inputs.head
    localSymFullName
  }
}
