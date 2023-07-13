package io.joern.kotlin2cpg.ast

import io.shiftleft.codepropertygraph.generated.{DispatchTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewJumpTarget, NewModifier, NewNamespaceBlock}

object Nodes {

  def modifierNode(_type: String): NewModifier = {
    NewModifier()
      .modifierType(_type)
  }

  def namespaceBlockNode(name: String, fullName: String, fileName: String): NewNamespaceBlock = {
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
  }

  def operatorCallNode(
    name: String,
    code: String,
    typeFullName: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewCall = {
    NewCall()
      .name(name)
      .methodFullName(name)
      .code(code)
      .signature("")
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .typeFullName(typeFullName.getOrElse("ANY"))
      .lineNumber(line)
      .columnNumber(column)
  }
}
