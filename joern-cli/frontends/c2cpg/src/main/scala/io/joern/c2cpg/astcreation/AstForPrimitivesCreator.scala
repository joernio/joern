package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.nodes.{NewComment, NewFieldIdentifier, NewIdentifier, NewLiteral}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.Ast
import org.eclipse.cdt.core.dom.ast._
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForPrimitivesCreator {

  this: AstCreator =>

  import AstCreatorHelper.OptionSafeAst

  protected def astForComment(comment: IASTComment): Ast =
    Ast(NewComment().code(nodeSignature(comment)).filename(fileName(comment)).lineNumber(line(comment)))

  protected def astForLiteral(lit: IASTLiteralExpression, argIndex: Int): Ast = {
    val tpe = cleanType(ASTTypeUtil.getType(lit.getExpressionType))
    val litNode = NewLiteral()
      .typeFullName(registerType(tpe))
      .code(nodeSignature(lit))
      .argumentIndex(argIndex)
      .lineNumber(line(lit))
      .columnNumber(column(lit))
    Ast(litNode)
  }

  protected def astForIdentifier(ident: IASTNode, argIndex: Int): Ast = {
    val identifierName = ident match {
      case id: IASTIdExpression => ASTStringUtil.getSimpleName(id.getName)
      case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty && id.getBinding != null => id.getBinding.getName
      case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty => uniqueName("name", "", "")._1
      case _                                                       => ident.toString
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

    val cpgIdentifier = NewIdentifier()
      .name(identifierName)
      .typeFullName(registerType(cleanType(identifierTypeName)))
      .code(nodeSignature(ident))
      .argumentIndex(argIndex)
      .lineNumber(line(ident))
      .columnNumber(column(ident))

    variableOption match {
      case Some((variable, _)) =>
        Ast(cpgIdentifier).withRefEdge(cpgIdentifier, variable)
      case None => Ast(cpgIdentifier)
    }
  }

  protected def astForFieldReference(fieldRef: IASTFieldReference, argIndex: Int): Ast = {
    val op    = if (fieldRef.isPointerDereference) Operators.indirectFieldAccess else Operators.fieldAccess
    val ma    = newCallNode(fieldRef, op, op, DispatchTypes.STATIC_DISPATCH, argIndex)
    val owner = astForExpression(fieldRef.getFieldOwner, 1)
    val member = NewFieldIdentifier()
      .canonicalName(fieldRef.getFieldName.toString)
      .code(fieldRef.getFieldName.toString)
      .argumentIndex(2)
      .lineNumber(line(fieldRef.getFieldName))
      .columnNumber(column(fieldRef.getFieldName))
    Ast(ma).withChild(owner).withChild(Ast(member)).withArgEdge(ma, owner.root).withArgEdge(ma, member)
  }

  protected def astForInitializerList(l: IASTInitializerList, argIndex: Int): Ast = {
    // TODO re-use from Operators once it there
    val op           = "<operator>.arrayInitializer"
    val initCallNode = newCallNode(l, op, op, DispatchTypes.STATIC_DISPATCH, argIndex)

    val MAX_INITIALIZERS = 1000
    val clauses          = l.getClauses.slice(0, MAX_INITIALIZERS)

    val args = withOrder(clauses) { case (c, o) =>
      astForNode(c, o)
    }
    val ast = Ast(initCallNode).withChildren(args).withArgEdges(initCallNode, args)
    if (l.getClauses.length > MAX_INITIALIZERS) {
      val placeholder = NewLiteral()
        .typeFullName("ANY")
        .code("<too-many-initializers>")
        .argumentIndex(MAX_INITIALIZERS)
        .lineNumber(line(l))
        .columnNumber(column(l))
      ast.withChild(Ast(placeholder)).withArgEdge(initCallNode, placeholder)
    } else {
      ast
    }
  }

  protected def astForQualifiedName(qualId: CPPASTQualifiedName, order: Int): Ast = {
    val op = Operators.fieldAccess
    val ma = newCallNode(qualId, op, op, DispatchTypes.STATIC_DISPATCH, order)

    def fieldAccesses(names: List[IASTNode], order: Int): Ast = names match {
      case Nil => Ast()
      case head :: Nil =>
        astForNode(head, order)
      case head :: tail =>
        val code     = s"${nodeSignature(head)}::${tail.map(nodeSignature).mkString("::")}"
        val callNode = newCallNode(head, op, op, DispatchTypes.STATIC_DISPATCH, order)
        callNode.code = code
        val arg1 = astForNode(head, 1)
        val arg2 = fieldAccesses(tail, 2)
        Ast(callNode)
          .withChild(arg1)
          .withChild(arg2)
          .withArgEdge(callNode, arg1.root)
          .withArgEdge(callNode, arg2.root)
    }

    val qualifier = fieldAccesses(qualId.getQualifier.toIndexedSeq.toList, 1)

    val owner = if (qualifier != Ast()) {
      qualifier
    } else {
      Ast(NewLiteral().code("<global>").argumentIndex(1).typeFullName("ANY"))
    }

    val member = NewFieldIdentifier()
      .canonicalName(qualId.getLastName.toString)
      .code(qualId.getLastName.toString)
      .argumentIndex(2)
      .lineNumber(line(qualId.getLastName))
      .columnNumber(column(qualId.getLastName))
    Ast(ma).withChild(owner).withChild(Ast(member)).withArgEdge(ma, member).withArgEdge(ma, owner.root)
  }

}
