package io.joern.c2cpg.astcreation

import io.joern.x2cpg.utils.NodeBuilders.methodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes._
import org.apache.commons.lang.StringUtils
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorIncludeStatement
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder { this: AstCreator =>

  protected def newFieldIdentifierNode(node: IASTNode, name: String, code: String): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newIdentifierNode(node: IASTNode, name: String, code: String, typeFullName: String): NewIdentifier = {
    NewIdentifier()
      .name(name)
      .typeFullName(typeFullName)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newLiteralNode(node: IASTNode, code: String, typeFullName: String): NewLiteral = {
    NewLiteral()
      .typeFullName(typeFullName)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newCommentNode(node: IASTNode, code: String, filename: String): NewComment = {
    NewComment().code(code).filename(filename).lineNumber(line(node)).columnNumber(column(node))
  }

  protected def newImportNode(
    code: String,
    importedEntity: String,
    include: IASTPreprocessorIncludeStatement
  ): NewImport = {
    NewImport()
      .code(code)
      .importedEntity(importedEntity)
      .importedAs(importedEntity)
      .lineNumber(line(include))
      .columnNumber(column(include))
  }

  protected def newLocalNode(node: IASTNode, name: String, code: String, typeFullName: String): NewLocal = {
    NewLocal()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newMemberNode(node: IASTNode, name: String, code: String, typeFullName: String): NewMember = {
    NewMember()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newNamespaceBlockNode(
    node: IASTNode,
    name: String,
    fullname: String,
    code: String,
    filename: String
  ): NewNamespaceBlock = {
    NewNamespaceBlock()
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .filename(filename)
      .name(name)
      .fullName(fullname)
  }

  protected def newBlockNode(node: IASTNode, typeFullName: String): NewBlock = {
    NewBlock()
      .lineNumber(line(node))
      .columnNumber(column(node))
      .typeFullName(typeFullName)
  }

  protected def newMethodReturnNode(node: IASTNode, typeFullName: String): NewMethodReturn = {
    methodReturnNode(typeFullName, None, line(node), column(node))
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
    NewMethodParameterIn()
      .name(name)
      .code(code)
      .typeFullName(typeFullName)
      .index(index)
      .evaluationStrategy(evaluationStrategy)
      .isVariadic(isVariadic)
      .lineNumber(line(node))
      .columnNumber(column(node))
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
    NewMethod()
      .name(name)
      .code(code)
      .fullName(fullName)
      .filename(fileName)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .lineNumberEnd(lineEnd(node))
      .columnNumberEnd(columnEnd(node))
      .astParentType(astParentType.getOrElse("<empty>"))
      .astParentFullName(astParentFullName.getOrElse("<empty>"))
  }

  protected def newMethodRefNode(
    code: String,
    methodFullName: String,
    typeFullName: String,
    node: IASTNode
  ): NewMethodRef = {
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(registerType(typeFullName))
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newCallNode(
    node: IASTNode,
    name: String,
    fullname: String,
    dispatchType: String,
    argIndex: Int = -1
  ): NewCall = {
    val callName     = StringUtils.normalizeSpace(name)
    val callFullName = StringUtils.normalizeSpace(fullname)
    NewCall()
      .name(callName)
      .dispatchType(dispatchType)
      .methodFullName(callFullName)
      .code(nodeSignature(node))
      .argumentIndex(argIndex)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def newJumpTargetNode(node: IASTNode): NewJumpTarget = {
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
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

}
