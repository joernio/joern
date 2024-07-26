package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.ICInternalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPInternalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPField
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
      // sadly, there is no common interface
      case b: ICInternalBinding if b.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
        Some(b.getDefinition.asInstanceOf[IASTFunctionDeclarator])
      case b: ICPPInternalBinding if b.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
        Some(b.getDefinition.asInstanceOf[IASTFunctionDeclarator])
      case b: ICInternalBinding   => b.getDeclarations.find(_.isInstanceOf[IASTFunctionDeclarator])
      case b: ICPPInternalBinding => b.getDeclarations.find(_.isInstanceOf[IASTFunctionDeclarator])
      case null                   => None
    }
    val typeFullName = definition.map(_.getParent) match {
      case Some(d: IASTFunctionDefinition) => Some(typeForDeclSpecifier(d.getDeclSpecifier))
      case Some(d: IASTSimpleDeclaration)  => Some(typeForDeclSpecifier(d.getDeclSpecifier))
      case _                               => None
    }
    (definition.map(fullName), typeFullName)
  }

  private def maybeMethodRefForIdentifier(ident: IASTNode): Option[NewMethodRef] = {
    ident match {
      case id: IASTIdExpression if id.getName != null =>
        id.getName.resolveBinding()
        val (mayBeFullName, mayBeTypeFullName) = id.getName.getBinding match {
          case binding: ICInternalBinding if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case binding: ICInternalBinding
              if binding.getDeclarations != null &&
                binding.getDeclarations.exists(_.isInstanceOf[IASTFunctionDeclarator]) =>
            namesForBinding(binding)
          case binding: ICPPInternalBinding if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case binding: ICPPInternalBinding
              if binding.getDeclarations != null &&
                binding.getDeclarations.exists(_.isInstanceOf[CPPASTFunctionDeclarator]) =>
            namesForBinding(binding)
          case _ => (None, None)
        }
        for {
          fullName     <- mayBeFullName
          typeFullName <- mayBeTypeFullName
        } yield methodRefNode(ident, code(ident), fullName, registerType(cleanType(typeFullName)))
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
          case None if ident.isInstanceOf[CPPASTIdExpression] =>
            ident.asInstanceOf[CPPASTIdExpression].getName.getBinding match {
              case f: CPPField => cleanType(f.getType.toString)
              case _           => typeFor(ident)
            }
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
    val ma     = callNode(fieldRef, code(fieldRef), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
    val owner  = astForExpression(fieldRef.getFieldOwner)
    val member = fieldIdentifierNode(fieldRef, fieldRef.getFieldName.toString, fieldRef.getFieldName.toString)
    callAst(ma, List(owner, Ast(member)))
  }

  protected def astForArrayModifier(arrMod: IASTArrayModifier): Ast =
    astForNode(arrMod.getConstantExpression)

  protected def astForInitializerList(l: IASTInitializerList): Ast = {
    val op           = Operators.arrayInitializer
    val initCallNode = callNode(l, code(l), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))

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
    val ma = callNode(qualId, code(qualId), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))

    def fieldAccesses(names: List[IASTNode], argIndex: Int = -1): Ast = names match {
      case Nil => Ast()
      case head :: Nil =>
        astForNode(head)
      case head :: tail =>
        val codeString = s"${code(head)}::${tail.map(code).mkString("::")}"
        val callNode_ =
          callNode(head, code(head), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(X2CpgDefines.Any))
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
