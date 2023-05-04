package io.joern.c2cpg.astcreation

import io.joern.x2cpg.utils.NodeBuilders.{newMethodReturnNode => newMethodReturnNode_}
import io.shiftleft.codepropertygraph.generated.nodes._
import org.apache.commons.lang.StringUtils
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorIncludeStatement
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder { this: AstCreator =>
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

  protected def newMethodReturnNode(node: IASTNode, typeFullName: String): NewMethodReturn = {
    newMethodReturnNode_(typeFullName, None, line(node), column(node))
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
}
