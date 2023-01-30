package io.joern.c2cpg.astcreation

import io.joern.x2cpg.utils.NodeBuilders.methodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes._
import org.apache.commons.lang.StringUtils
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder { this: AstCreator =>

  protected def newUnknownNode(node: IASTNode): NewUnknown = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewUnknown()
      .parserTypeName(node.getClass.getSimpleName)
      .code(nodeSignature(node))
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newFieldIdentifierNode(node: IASTNode, name: String, code: String): NewFieldIdentifier = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewFieldIdentifier()
      .canonicalName(name)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newIdentifierNode(node: IASTNode, name: String, code: String, typeFullName: String): NewIdentifier = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewIdentifier()
      .name(name)
      .typeFullName(typeFullName)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newLiteralNode(node: IASTNode, code: String, typeFullName: String): NewLiteral = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewLiteral()
      .typeFullName(typeFullName)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newCommentNode(node: IASTNode, code: String, filename: String): NewComment = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewComment().code(code).filename(filename).lineNumber(lineNumber).columnNumber(columnNumber)
  }

  protected def newLocalNode(node: IASTNode, name: String, code: String, typeFullName: String): NewLocal = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewLocal()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newMemberNode(node: IASTNode, name: String, code: String, typeFullName: String): NewMember = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewMember()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newNamespaceBlockNode(
    node: IASTNode,
    name: String,
    fullname: String,
    code: String,
    filename: String
  ): NewNamespaceBlock = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewNamespaceBlock()
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .filename(filename)
      .name(name)
      .fullName(fullname)
  }

  protected def newBlockNode(node: IASTNode, typeFullName: String): NewBlock = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewBlock()
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .typeFullName(typeFullName)
  }

  protected def newMethodReturnNode(node: IASTNode, typeFullName: String): NewMethodReturn = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    methodReturnNode(typeFullName, None, lineNumber, columnNumber)
  }

  protected def newReturnNode(node: IASTNode, code: String): NewReturn = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewReturn().code(code).lineNumber(lineNumber).columnNumber(columnNumber)
  }

  protected def newParameterInNode(
    node: IASTNode,
    name: String,
    code: String,
    typeFullName: String,
    index: Int,
    evaluationStrategy: String,
    isVariadic: Boolean
  ): NewMethodParameterIn = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewMethodParameterIn()
      .name(name)
      .code(code)
      .typeFullName(typeFullName)
      .index(index)
      .evaluationStrategy(evaluationStrategy)
      .isVariadic(isVariadic)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newMethodNode(
    node: IASTNode,
    name: String,
    code: String,
    fullName: String,
    fileName: String,
    astParentType: Option[String] = None,
    astParentFullName: Option[String] = None
  ): NewMethod = {
    val lineNumber      = line(node).map(Integer.valueOf)
    val lineNumberEnd   = lineEnd(node).map(Integer.valueOf)
    val columnNumber    = column(node).map(Integer.valueOf)
    val columnNumberEnd = columnEnd(node).map(Integer.valueOf)
    NewMethod()
      .name(name)
      .code(code)
      .fullName(fullName)
      .filename(fileName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .lineNumberEnd(lineNumberEnd)
      .columnNumberEnd(columnNumberEnd)
      .astParentType(astParentType.getOrElse("<empty>"))
      .astParentFullName(astParentFullName.getOrElse("<empty>"))
  }

  protected def newMethodRefNode(
    code: String,
    methodFullName: String,
    typeFullName: String,
    node: IASTNode
  ): NewMethodRef = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(registerType(typeFullName))
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newCallNode(
    node: IASTNode,
    name: String,
    fullname: String,
    dispatchType: String,
    argIndex: Int = -1
  ): NewCall = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    val callName     = StringUtils.normalizeSpace(name)
    val callFullName = StringUtils.normalizeSpace(fullname)
    NewCall()
      .name(callName)
      .dispatchType(dispatchType)
      .methodFullName(callFullName)
      .code(nodeSignature(node))
      .argumentIndex(argIndex)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newControlStructureNode(
    node: IASTNode,
    controlStructureType: String,
    code: String
  ): NewControlStructure = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newJumpTargetNode(node: IASTNode): NewJumpTarget = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    val code         = nodeSignature(node)
    val name = node match {
      case label: IASTLabelStatement    => ASTStringUtil.getSimpleName(label.getName)
      case _ if code.startsWith("case") => "case"
      case _                            => "default"
    }
    NewJumpTarget()
      .parserTypeName(node.getClass.getSimpleName)
      .name(name)
      .code(code)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def newTypeDeclNode(
    node: IASTNode,
    name: String,
    fullName: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None
  ): NewTypeDecl = {
    val lineNumber   = line(node).map(Integer.valueOf)
    val columnNumber = column(node).map(Integer.valueOf)
    NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

}
