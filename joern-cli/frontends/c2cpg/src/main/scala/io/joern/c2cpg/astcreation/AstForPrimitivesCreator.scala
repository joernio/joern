package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForPrimitivesCreator { this: AstCreator =>

  protected def astForComment(comment: IASTComment): Ast =
    Ast(newCommentNode(comment, nodeSignature(comment), fileName(comment)))

  protected def astForLiteral(lit: IASTLiteralExpression): Ast = {
    val tpe = cleanType(ASTTypeUtil.getType(lit.getExpressionType))
    Ast(literalNode(lit, nodeSignature(lit), registerType(tpe)))
  }

  protected def astForIdentifier(ident: IASTNode): Ast = {
    val identifierName = ident match {
      case id: IASTIdExpression => ASTStringUtil.getSimpleName(id.getName)
      case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty && id.getBinding != null => id.getBinding.getName
      case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty => uniqueName("name", "", "")._1
      case _                                                       => nodeSignature(ident)
    }
    val variableOption = scope.lookupVariable(identifierName)
    val identifierTypeName = variableOption match {
      case Some((_, variableTypeName)) => variableTypeName
      case None if ident.isInstanceOf[IASTName] && ident.asInstanceOf[IASTName].getBinding != null =>
        val id = ident.asInstanceOf[IASTName]
        id.getBinding match {
          case v: IVariable =>
            v.getType match {
              case f: IFunctionType => f.getReturnType.toString
              case other            => other.toString
            }
          case other => other.getName
        }
      case None if ident.isInstanceOf[IASTName] =>
        typeFor(ident.getParent)
      case None => typeFor(ident)
    }

    val node = identifierNode(ident, identifierName, nodeSignature(ident), registerType(cleanType(identifierTypeName)))
    variableOption match {
      case Some((variable, _)) =>
        Ast(node).withRefEdge(node, variable)
      case None => Ast(node)
    }
  }

  protected def astForFieldReference(fieldRef: IASTFieldReference): Ast = {
    val op     = if (fieldRef.isPointerDereference) Operators.indirectFieldAccess else Operators.fieldAccess
    val ma     = newCallNode(fieldRef, op, op, DispatchTypes.STATIC_DISPATCH)
    val owner  = astForExpression(fieldRef.getFieldOwner)
    val member = fieldIdentifierNode(fieldRef, fieldRef.getFieldName.toString, fieldRef.getFieldName.toString)
    callAst(ma, List(owner, Ast(member)))
  }

  protected def astForArrayModifier(arrMod: IASTArrayModifier): Ast =
    astForNode(arrMod.getConstantExpression)

  protected def astForInitializerList(l: IASTInitializerList): Ast = {
    val op           = Operators.arrayInitializer
    val initCallNode = newCallNode(l, op, op, DispatchTypes.STATIC_DISPATCH)

    val MAX_INITIALIZERS = 1000
    val clauses          = l.getClauses.slice(0, MAX_INITIALIZERS)

    val args = clauses.toList.map(x => astForNode(x))

    val ast = callAst(initCallNode, args)
    if (l.getClauses.length > MAX_INITIALIZERS) {
      val placeholder =
        literalNode(l, "<too-many-initializers>", Defines.anyTypeName).argumentIndex(MAX_INITIALIZERS)
      ast.withChild(Ast(placeholder)).withArgEdge(initCallNode, placeholder)
    } else {
      ast
    }
  }

  protected def astForQualifiedName(qualId: CPPASTQualifiedName): Ast = {
    val op = Operators.fieldAccess
    val ma = newCallNode(qualId, op, op, DispatchTypes.STATIC_DISPATCH)

    def fieldAccesses(names: List[IASTNode], argIndex: Int = -1): Ast = names match {
      case Nil => Ast()
      case head :: Nil =>
        astForNode(head)
      case head :: tail =>
        val code     = s"${nodeSignature(head)}::${tail.map(nodeSignature).mkString("::")}"
        val callNode = newCallNode(head, op, op, DispatchTypes.STATIC_DISPATCH, argIndex)
        callNode.code = code
        val arg1 = astForNode(head)
        val arg2 = fieldAccesses(tail)
        callAst(callNode, List(arg1, arg2))
    }

    val qualifier = fieldAccesses(qualId.getQualifier.toIndexedSeq.toList)

    val owner = if (qualifier != Ast()) {
      qualifier
    } else {
      Ast(literalNode(qualId.getLastName, "<global>", Defines.anyTypeName))
    }

    val member = fieldIdentifierNode(
      qualId.getLastName,
      fixQualifiedName(qualId.getLastName.toString),
      qualId.getLastName.toString
    )
    callAst(ma, List(owner, Ast(member)))
  }

}
