package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: IASTNode, order: Int): NewUnknown =
    NewUnknown()
      .parserTypeName(node.getClass.getSimpleName)
      .code(nodeSignature(node))
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newMethodRefNode(
      code: String,
      methodFullName: String,
      typeFullName: String,
      node: IASTNode
  ): NewMethodRef =
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(registerType(typeFullName))
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newCallNode(
      astNode: IASTNode,
      name: String,
      fullname: String,
      dispatchType: String,
      order: Int
  ): NewCall = {
    NewCall()
      .name(name)
      .dispatchType(dispatchType)
      .signature("TODO")
      .methodFullName(fullname)
      .code(nodeSignature(astNode))
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(astNode))
      .columnNumber(column(astNode))
  }

  protected def newControlStructureNode(
      node: IASTNode,
      controlStructureType: String,
      code: String,
      order: Int
  ): NewControlStructure =
    NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newJumpTarget(node: IASTNode, order: Int): NewJumpTarget = {
    val code = nodeSignature(node)
    val name = node match {
      case label: IASTLabelStatement    => ASTStringUtil.getSimpleName(label.getName)
      case _ if code.startsWith("case") => "case"
      case _                            => "default"
    }
    NewJumpTarget()
      .parserTypeName(node.getClass.getSimpleName)
      .name(name)
      .code(code)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newTypeDecl(
      name: String,
      fullname: String,
      filename: String,
      code: String,
      astParentType: String = "",
      astParentFullName: String = "",
      order: Int = -1,
      inherits: Seq[String] = Seq.empty,
      alias: Option[String] = None,
      line: Option[Integer] = None,
      column: Option[Integer] = None
  ): NewTypeDecl =
    NewTypeDecl()
      .name(name)
      .fullName(fullname)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits.map(registerType))
      .aliasTypeFullName(alias.map(registerType))
      .lineNumber(line)
      .columnNumber(column)
      .order(order)

}
