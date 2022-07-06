package io.joern.x2cpg.utils

import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewIdentifier, NewModifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

object NodeBuilders {
  def assignmentNode(): NewCall = NewCall()
    .name(Operators.assignment)
    .methodFullName(Operators.assignment)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)

  def indexAccessNode(): NewCall = NewCall()
    .name(Operators.indexAccess)
    .methodFullName(Operators.indexAccess)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)

  def identifierNode(
    name: String,
    typeFullName: String,
    line: Option[Integer] = None,
    column: Option[Integer] = None,
    dynamicTypeHintFullName: Seq[String] = Seq.empty
  ): NewIdentifier = NewIdentifier()
    .name(name)
    .code(name)
    .typeFullName(typeFullName)
    .lineNumber(line)
    .columnNumber(column)
    .dynamicTypeHintFullName(dynamicTypeHintFullName)

  def modifierNode(modifierType: String): NewModifier = NewModifier().modifierType(modifierType)
}
