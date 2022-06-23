package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import org.apache.commons.lang.StringUtils
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: IASTNode): NewUnknown =
    NewUnknown()
      .parserTypeName(node.getClass.getSimpleName)
      .code(nodeSignature(node))
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
    node: IASTNode,
    name: String,
    fullname: String,
    dispatchType: String,
    argIndex: Int = -1
  ): NewCall = {
    NewCall()
      .name(StringUtils.normalizeSpace(name))
      .dispatchType(dispatchType)
      .methodFullName(StringUtils.normalizeSpace(fullname))
      .code(nodeSignature(node))
      .argumentIndex(argIndex)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newControlStructureNode(
    node: IASTNode,
    controlStructureType: String,
    code: String
  ): NewControlStructure =
    NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newJumpTarget(node: IASTNode): NewJumpTarget = {
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
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line)
      .columnNumber(column)

}
