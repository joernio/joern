package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewCall}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

/** Mixin that provides helpers for building call ASTs. */
private[x2cpg] trait CallAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

  /** Create an abstract syntax tree for a call, including CPG-specific edges required for arguments and the receiver.
    *
    * Our call representation is inspired by ECMAScript, that is, in addition to arguments, a call has a base and a
    * receiver. For languages other than Javascript, leave `receiver` empty for now.
    *
    * @param callNode
    *   the node that represents the entire call
    * @param arguments
    *   arguments (without the base argument (instance))
    * @param base
    *   the value to use as `this` in the method call.
    * @param receiver
    *   the object in which the property lookup is performed
    */
  def callAst(
    callNode: NewCall,
    arguments: Seq[Ast] = List(),
    base: Option[Ast] = None,
    receiver: Option[Ast] = None
  ): Ast = {
    setArgumentIndices(arguments)

    val baseRoot = base.flatMap(_.root).toList
    val bse      = base.getOrElse(Ast())
    baseRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
      case _ =>
    }

    val receiverRoot = if (receiver.isEmpty && base.nonEmpty) {
      baseRoot
    } else {
      val receiverList = receiver.flatMap(_.root).toList
      receiverList match {
        case List(expr: ExpressionNew) =>
          expr.argumentIndex = -1
        case _ =>
      }
      receiverList
    }

    val rcvAst = receiver.getOrElse(Ast())

    Ast(callNode)
      .withChild(rcvAst)
      .withChild(bse)
      .withChildren(arguments)
      .withArgEdges(callNode, baseRoot)
      .withArgEdges(callNode, arguments.flatMap(_.root))
      .withReceiverEdges(callNode, receiverRoot)
  }

  /** Creates an AST for a field-access expression (`base.fieldName`).
    *
    * Emits a [[io.shiftleft.codepropertygraph.generated.Operators.fieldAccess]] call node with `base` as the first
    * argument and a [[io.shiftleft.codepropertygraph.generated.nodes.NewFieldIdentifier]] for `fieldName` as the second
    * argument.
    *
    * @param originNode
    *   the source AST node that represents the entire field-access expression
    * @param fieldIdentifierOrigin
    *   the source AST node that represents the field name (used for position information)
    * @param base
    *   AST of the object whose field is accessed
    * @param code
    *   source code string of the whole expression
    * @param fieldName
    *   the name of the field being accessed
    * @param fieldTypeFullName
    *   fully-qualified type of the accessed field
    */
  def fieldAccessAst(
    originNode: Node,
    fieldIdentifierOrigin: Node,
    base: Ast,
    code: String,
    fieldName: String,
    fieldTypeFullName: String
  ): Ast = {
    val callNode_ = callNode(
      originNode,
      code,
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(fieldTypeFullName)
    )
    val fieldIdentifierNode_ = fieldIdentifierNode(fieldIdentifierOrigin, fieldName, fieldName)
    callAst(callNode_, Seq(base, Ast(fieldIdentifierNode_)))
  }
}
