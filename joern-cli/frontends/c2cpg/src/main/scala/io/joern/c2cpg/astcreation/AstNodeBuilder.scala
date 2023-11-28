package io.joern.c2cpg.astcreation

import io.joern.x2cpg.utils.NodeBuilders.{newMethodReturnNode => newMethodReturnNode_}
import io.shiftleft.codepropertygraph.generated.nodes._
import org.eclipse.cdt.core.dom.ast.{IASTLabelStatement, IASTNode}
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorIncludeStatement
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstNodeBuilder { this: AstCreator =>
  protected def newCommentNode(node: IASTNode, code: String, filename: String): NewComment = {
    NewComment().code(code).filename(filename).lineNumber(line(node)).columnNumber(column(node))
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

  // TODO: We should get rid of this method as its being used at multiple places and use it from x2cpg/AstNodeBuilder "methodReturnNode"
  protected def newMethodReturnNode(node: IASTNode, typeFullName: String): NewMethodReturn = {
    newMethodReturnNode_(typeFullName, None, line(node), column(node))
  }

  protected def newJumpTargetNode(node: IASTNode): NewJumpTarget = {
    val codeString = code(node)
    val name = node match {
      case label: IASTLabelStatement          => ASTStringUtil.getSimpleName(label.getName)
      case _ if codeString.startsWith("case") => "case"
      case _                                  => "default"
    }
    NewJumpTarget()
      .parserTypeName(node.getClass.getSimpleName)
      .name(name)
      .code(codeString)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }
}
