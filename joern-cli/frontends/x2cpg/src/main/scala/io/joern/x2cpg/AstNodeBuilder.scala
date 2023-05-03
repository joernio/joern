package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewControlStructure,
  NewFieldIdentifier,
  NewMethodRef,
  NewReturn,
  NewTypeRef,
  NewUnknown
}

trait AstNodeBuilder[Node, NodeProcessor] { this: NodeProcessor =>
  protected def line(node: Node): Option[Integer]
  protected def column(node: Node): Option[Integer]
  protected def lineEnd(node: Node): Option[Integer]
  protected def columnEnd(element: Node): Option[Integer]

  protected def unknownNode(node: Node, code: String): NewUnknown = {
    NewUnknown()
      .parserTypeName(node.getClass.getSimpleName)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def methodRefNode(node: Node, code: String, methodFullName: String, typeFullName: String): NewMethodRef = {
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def typeRefNode(node: Node, code: String, typeFullName: String): NewTypeRef = {
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def returnNode(node: Node, code: String): NewReturn = {
    NewReturn()
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def controlStructureNode(node: Node, controlStructureType: String, code: String): NewControlStructure = {
    NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def blockNode(node: Node, code: String, typeFullName: String): NewBlock = {
    NewBlock()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def fieldIdentifierNode(node: Node, name: String, code: String): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }
}
