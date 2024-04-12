package io.shiftleft.semanticcpg.typeprop
import io.shiftleft.codepropertygraph.generated.{Operators, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.semanticcpg.language.*

class SwiftLanguage(symbolProvider: SymbolProviderInterface) extends CpgLanguageInterface {

  override def inputsOff(output: nodes.StoredNode): InputsAndTransferFunc[nodes.StoredNode] = {
    output match {
      case call: nodes.Call if call.methodFullName == Operators.assignment =>
        
      case call: nodes.Call =>
        val receiverAndArguments = call.receiver ++ call.argument
        new InputsAndTransferFunc(receiverAndArguments.toBuffer, handleCall(call.methodFullName))
      case identifier: nodes.Identifier =>
        val local = identifier.refsTo.take(1)
        new InputsAndTransferFunc(local.toBuffer, handleIdentifier)
      case local: nodes.Local =>
        // TODO use DFO here. We need the initialising assignment to a local variable
        val firstIdentifier = local.referencingIdentifiers.sortBy(_.lineNumber).take(1)
        // Parent call defines the local type. So far the assumption is that this
        // parent call is always an initialisation assignment.
        val parentCall = firstIdentifier.astParent.isCall
        new InputsAndTransferFunc(parentCall.toBuffer, handleLocal)
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

  private def handleLocal(inputs: collection.Seq[Option[String]]): Option[String] = {
    val assignmentRhsSymFullName = inputs.head
    assignmentRhsSymFullName
  }
}
