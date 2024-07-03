package io.joern.c2cpg.astcreation

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.ICInternalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPInternalBinding
import org.eclipse.cdt.internal.core.model.ASTStringUtil

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForComment(comment: IASTComment): Ast =
    Ast(newCommentNode(comment, code(comment), fileName(comment)))

  protected def astForLiteral(lit: IASTLiteralExpression): Ast = {
    val tpe = cleanType(safeGetType(lit.getExpressionType))
    Ast(literalNode(lit, code(lit), registerType(tpe)))
  }

  private def namesForBinding(binding: ICInternalBinding | ICPPInternalBinding): (Option[String], Option[String]) = {
    val definition = binding match {
      // sadly, there is no common interface defining .getDefinition
      case b: ICInternalBinding   => b.getDefinition.asInstanceOf[IASTFunctionDeclarator]
      case b: ICPPInternalBinding => b.getDefinition.asInstanceOf[IASTFunctionDeclarator]
    }
    val typeFullName = definition.getParent match {
      case d: IASTFunctionDefinition => Some(typeForDeclSpecifier(d.getDeclSpecifier))
      case _                         => None
    }
    (Some(this.fullName(definition)), typeFullName)
  }

  private def maybeMethodRefForIdentifier(ident: IASTNode): Option[NewMethodRef] = {
    ident match {
      case id: IASTIdExpression if id.getName != null =>
        id.getName.resolveBinding()
        val (mayBeFullName, mayBeTypeFullName) = id.getName.getBinding match {
          case binding: ICInternalBinding if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case binding: ICPPInternalBinding if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case _ => (None, None)
        }
        for {
          fullName     <- mayBeFullName
          typeFullName <- mayBeTypeFullName
        } yield methodRefNode(ident, code(ident), fullName, typeFullName)
      case _ => None
    }
  }

  protected def astForIdentifier(ident: IASTNode): Ast = {
    maybeMethodRefForIdentifier(ident) match {
      case Some(ref) => Ast(ref)
      case None =>
        val identifierName = ident match {
          case id: IASTIdExpression => ASTStringUtil.getSimpleName(id.getName)
          case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty && id.getBinding != null => id.getBinding.getName
          case id: IASTName if ASTStringUtil.getSimpleName(id).isEmpty => uniqueName("name", "", "")._1
          case _                                                       => code(ident)
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

        val node = identifierNode(ident, identifierName, code(ident), registerType(cleanType(identifierTypeName)))
        variableOption match {
          case Some((variable, _)) =>
            Ast(node).withRefEdge(node, variable)
          case None => Ast(node)
        }
    }
  }

  protected def astForFieldReference(fieldRef: IASTFieldReference): Ast = {
    val op     = if (fieldRef.isPointerDereference) Operators.indirectFieldAccess else Operators.fieldAccess
    val ma     = callNode(fieldRef, code(fieldRef), op, op, DispatchTypes.STATIC_DISPATCH)
    val owner  = astForExpression(fieldRef.getFieldOwner)
    val member = fieldIdentifierNode(fieldRef, fieldRef.getFieldName.toString, fieldRef.getFieldName.toString)
    callAst(ma, List(owner, Ast(member)))
  }

  protected def astForArrayModifier(arrMod: IASTArrayModifier): Ast =
    astForNode(arrMod.getConstantExpression)

  protected def astForInitializerList(l: IASTInitializerList): Ast = {
    val op           = Operators.arrayInitializer
    val initCallNode = callNode(l, code(l), op, op, DispatchTypes.STATIC_DISPATCH)

    val MAX_INITIALIZERS = 1000
    val clauses          = l.getClauses.slice(0, MAX_INITIALIZERS)

    val args = clauses.toList.map(x => astForNode(x))

    val ast = callAst(initCallNode, args)
    if (l.getClauses.length > MAX_INITIALIZERS) {
      val placeholder =
        literalNode(l, "<too-many-initializers>", Defines.Any).argumentIndex(MAX_INITIALIZERS)
      ast.withChild(Ast(placeholder)).withArgEdge(initCallNode, placeholder)
    } else {
      ast
    }
  }

  protected def astForQualifiedName(qualId: CPPASTQualifiedName): Ast = {
    val op = Operators.fieldAccess
    val ma = callNode(qualId, code(qualId), op, op, DispatchTypes.STATIC_DISPATCH)

    def fieldAccesses(names: List[IASTNode], argIndex: Int = -1): Ast = names match {
      case Nil => Ast()
      case head :: Nil =>
        astForNode(head)
      case head :: tail =>
        val codeString = s"${code(head)}::${tail.map(code).mkString("::")}"
        val callNode_ =
          callNode(head, code(head), op, op, DispatchTypes.STATIC_DISPATCH)
            .argumentIndex(argIndex)
        callNode_.code = codeString
        val arg1 = astForNode(head)
        val arg2 = fieldAccesses(tail)
        callAst(callNode_, List(arg1, arg2))
    }

    val qualifier = fieldAccesses(qualId.getQualifier.toIndexedSeq.toList)

    val owner = if (qualifier != Ast()) {
      qualifier
    } else {
      Ast(literalNode(qualId.getLastName, "<global>", Defines.Any))
    }

    val member = fieldIdentifierNode(
      qualId.getLastName,
      fixQualifiedName(qualId.getLastName.toString),
      qualId.getLastName.toString
    )
    callAst(ma, List(owner, Ast(member)))
  }

}
