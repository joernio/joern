package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction
import org.eclipse.cdt.internal.core.dom.parser.c.ICInternalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPField
import org.eclipse.cdt.internal.core.dom.parser.cpp.ICPPInternalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPVisitor
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalMemberAccess
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.util.Try

trait AstForPrimitivesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForComment(comment: IASTComment): Ast =
    Ast(newCommentNode(comment, code(comment), fileName(comment)))

  protected def astForLiteral(lit: IASTLiteralExpression): Ast = {
    val codeString = code(lit)
    val tpe        = registerType(cleanType(safeGetType(lit.getExpressionType)))
    if (codeString == "this") {
      val thisIdentifier = identifierNode(lit, codeString, codeString, tpe)
      scope.addVariableReference(codeString, thisIdentifier, tpe, EvaluationStrategies.BY_REFERENCE)
      Ast(thisIdentifier)
    } else {
      Ast(literalNode(lit, codeString, tpe))
    }
  }

  protected def astForIdentifier(ident: IASTNode): Ast = {
    maybeMethodRefForIdentifier(ident) match {
      case Some(ref) => Ast(ref)
      case None =>
        val identifierName = nameForIdentifier(ident)
        typeNameForIdentifier(ident, identifierName) match {
          case identifierTypeName: String =>
            val tpe  = registerType(cleanType(identifierTypeName))
            val node = identifierNode(ident, identifierName, code(ident), tpe)
            scope.addVariableReference(identifierName, node, tpe, EvaluationStrategies.BY_REFERENCE)
            Ast(node)
          case ast: Ast => ast
        }
    }
  }

  private def maybeMethodRefForIdentifier(ident: IASTNode): Option[NewMethodRef] = {
    ident match {
      case id: IASTIdExpression if id.getName != null =>
        val (mayBeFullName, mayBeTypeFullName) = safeGetBinding(id) match {
          case Some(binding: ICInternalBinding) if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case Some(binding: ICInternalBinding)
              if binding.getDeclarations != null &&
                binding.getDeclarations.exists(_.isInstanceOf[IASTFunctionDeclarator]) =>
            namesForBinding(binding)
          case Some(binding: ICPPInternalBinding) if binding.getDefinition.isInstanceOf[IASTFunctionDeclarator] =>
            namesForBinding(binding)
          case Some(binding: ICPPInternalBinding)
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

  private def nameForIdentifier(ident: IASTNode): String = {
    ident match {
      case id: IASTElaboratedTypeSpecifier => ASTStringUtil.getSimpleName(id.getName)
      case id: IASTNamedTypeSpecifier      => ASTStringUtil.getSimpleName(id.getName)
      case id: IASTIdExpression            => ASTStringUtil.getSimpleName(id.getName)
      case id: IASTName =>
        val name = ASTStringUtil.getSimpleName(id)
        if (name.isEmpty) safeGetBinding(id).map(_.getName).getOrElse(uniqueName("", "")._1)
        else name
      case _ => code(ident)
    }
  }

  private def typeNameForIdentifier(ident: IASTNode, identifierName: String): String | Ast = {
    val variableOption = scope.lookupVariable(identifierName)
    variableOption match {
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
        syntheticThisAccess(ident.asInstanceOf[CPPASTIdExpression], identifierName)
      case None => typeFor(ident)
    }
  }

  private def syntheticThisAccess(ident: CPPASTIdExpression, identifierName: String): String | Ast = {
    val tpe = ident.getName.getBinding match {
      case f: CPPField => safeGetType(f.getType)
      case _           => typeFor(ident)
    }
    Try(ident.getEvaluation).toOption match {
      case Some(e: EvalMemberAccess) =>
        val ownerTypeRaw = cleanType(safeGetType(e.getOwnerType))
        val deref        = if (e.isPointerDeref) "*" else ""
        val ownerType    = registerType(s"$ownerTypeRaw$deref")
        if (isInCurrentScope(ident, ownerTypeRaw)) {
          scope.lookupVariable("this") match {
            case Some(_) =>
              val op             = Operators.indirectFieldAccess
              val code           = s"this->$identifierName"
              val thisIdentifier = identifierNode(ident, "this", "this", ownerType)
              scope.addVariableReference("this", thisIdentifier, ownerType, EvaluationStrategies.BY_REFERENCE)
              val member  = fieldIdentifierNode(ident, identifierName, identifierName)
              val callTpe = Some(registerType(cleanType(tpe)))
              val ma      = callNode(ident, code, op, op, DispatchTypes.STATIC_DISPATCH, None, callTpe)
              callAst(ma, Seq(Ast(thisIdentifier), Ast(member)))
            case None => tpe
          }
        } else tpe
      case _ => tpe
    }
  }

  private def isInCurrentScope(ident: CPPASTIdExpression, owner: String): Boolean = {
    val ownerWithOutTemplateTags = owner.takeWhile(_ != '<')
    val isInMethodScope =
      Try(CPPVisitor.getContainingScope(ident).getScopeName.toString).toOption.exists(s =>
        s.startsWith(s"$ownerWithOutTemplateTags::") || s.contains(s"::$ownerWithOutTemplateTags::")
      )
    isInMethodScope || methodAstParentStack.collectFirst {
      case typeDecl: NewTypeDecl if typeDecl.fullName == ownerWithOutTemplateTags    => typeDecl
      case method: NewMethod if method.fullName.startsWith(ownerWithOutTemplateTags) => method
    }.nonEmpty
  }

  protected def astForFieldReference(fieldRef: IASTFieldReference): Ast = {
    val op     = if (fieldRef.isPointerDereference) Operators.indirectFieldAccess else Operators.fieldAccess
    val ma     = callNode(fieldRef, code(fieldRef), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val owner  = astForExpression(fieldRef.getFieldOwner)
    val member = fieldIdentifierNode(fieldRef, fieldRef.getFieldName.toString, fieldRef.getFieldName.toString)
    callAst(ma, List(owner, Ast(member)))
  }

  protected def astForArrayModifier(arrMod: IASTArrayModifier): Ast = {
    astForNode(arrMod.getConstantExpression)
  }

  protected def astForQualifiedName(qualId: CPPASTQualifiedName): Ast = {
    safeGetBinding(qualId) match {
      case Some(function: ICPPFunction) =>
        val name      = qualId.getLastName.toString
        val signature = if function.isExternC then "" else functionTypeToSignature(function.getType)
        val fullName = if (function.isExternC) {
          StringUtils.normalizeSpace(name)
        } else {
          val fullNameNoSig = StringUtils.normalizeSpace(function.getQualifiedName.mkString("."))
          s"$fullNameNoSig:$signature"
        }
        Ast(methodRefNode(qualId, name, fullName, registerType(cleanType(function.getType.toString))))
      case _ =>
        val op = Operators.fieldAccess
        val ma = callNode(qualId, code(qualId), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))

        def fieldAccesses(names: List[IASTNode], argIndex: Int = -1): Ast = names match {
          case Nil => Ast()
          case head :: Nil =>
            astForNode(head)
          case head :: tail =>
            val codeString = s"${code(head)}::${tail.map(code).mkString("::")}"
            val callNode_ =
              callNode(head, code(head), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
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

}
