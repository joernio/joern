package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewMethodRef, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, Operators}
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.{ICPPASTNamespaceDefinition, ICPPConstructor, ICPPFunction}
import org.eclipse.cdt.internal.core.dom.parser.IASTInternalScope
import org.eclipse.cdt.internal.core.dom.parser.c.{CVariable, ICInternalBinding}
import org.eclipse.cdt.internal.core.dom.parser.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.{CPPVisitor, EvalMemberAccess}
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.tailrec
import scala.util.Try

trait AstForPrimitivesCreator { this: AstCreator =>

  import FullNameProvider.stripTemplateTags

  protected def astForComment(comment: IASTComment): Ast =
    Ast(commentNode(comment, code(comment), fileName(comment)))

  protected def astForLiteral(lit: IASTLiteralExpression): Ast = {
    val codeString = code(lit)
    val tpe        = registerType(safeGetType(lit.getExpressionType))
    if (codeString == Defines.This) {
      val thisIdentifier = identifierNode(lit, codeString, codeString, tpe)
      scope.addVariableReference(codeString, thisIdentifier, tpe, EvaluationStrategies.BY_SHARING)
      Ast(thisIdentifier)
    } else {
      Ast(literalNode(lit, codeString, tpe))
    }
  }

  private def globalTagFromDefinition(ident: IASTNode): String = {
    @tailrec
    def isGlobal(node: IASTNode): Boolean = {
      node match {
        case _: IASTTranslationUnit        => true
        case n: ICPPASTNamespaceDefinition => isGlobal(n.getParent)
        case _                             => false
      }
    }
    ident match {
      case id: IASTIdExpression =>
        safeGetBinding(id.getName) match {
          case Some(binding: (CPPVariable | CVariable)) =>
            Try(binding.getScope).toOption
              .collect {
                case n: IASTInternalScope if isGlobal(n.getPhysicalNode) => s"${Defines.GlobalTag} "
              }
              .getOrElse("")
          case Some(p: IProblemBinding)
              if p.getID == ISemanticProblem.BINDING_NOT_FOUND ||
                p.getID == ISemanticProblem.BINDING_AMBIGUOUS_LOOKUP ||
                p.getID == ISemanticProblem.BINDING_BAD_SCOPE ||
                p.getID == ISemanticProblem.BINDING_MEMBER_DECLARATION_NOT_FOUND ||
                p.getID == ISemanticProblem.BINDING_DEFINITION_NOT_FOUND =>
            s"${Defines.UnknownTag} "
          case _ => ""
        }
      case _ => ""
    }
  }

  protected def astForIdentifier(ident: IASTNode): Ast = {
    maybeMethodRefForIdentifier(ident) match {
      case Some(ref) => Ast(ref)
      case None =>
        val identifierName = nameForIdentifier(ident)
        typeNameForIdentifier(ident, identifierName) match {
          case identifierTypeName: String =>
            val tpe       = registerType(identifierTypeName)
            val globalTag = globalTagFromDefinition(ident)
            val node      = identifierNode(ident, identifierName, s"$globalTag${code(ident)}", tpe)
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
        } yield methodRefNode(ident, code(ident), fullName, registerType(typeFullName))
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

  protected def nameForIdentifier(ident: IASTNode): String = {
    ident match {
      case id: IASTElaboratedTypeSpecifier => shortName(id)
      case id: IASTNamedTypeSpecifier      => shortName(id)
      case id: IASTIdExpression            => shortName(id)
      case id: IASTName =>
        val name = stripTemplateTags(ASTStringUtil.getSimpleName(id))
        if (name.isEmpty) safeGetBinding(id).map(_.getName).getOrElse(scopeLocalUniqueName(""))
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
              case f: IFunctionType => cleanType(f.getReturnType.toString)
              case other            => cleanType(other.toString)
            }
          case other => cleanType(other.getName)
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
        val ownerTypeRaw = safeGetType(e.getOwnerType)
        val deref        = if (e.isPointerDeref) "*" else ""
        val ownerType    = registerType(s"$ownerTypeRaw$deref")
        if (isInCurrentScope(ident, ownerTypeRaw)) {
          scope.lookupVariable(Defines.This) match {
            case Some(_) =>
              val (op, code) = if (e.isPointerDeref) {
                (Operators.indirectFieldAccess, s"${Defines.This}->$identifierName")
              } else {
                (Operators.fieldAccess, s"${Defines.This}.$identifierName")
              }
              val thisIdentifier = identifierNode(ident, Defines.This, Defines.This, ownerType)
              scope.addVariableReference(Defines.This, thisIdentifier, ownerType, EvaluationStrategies.BY_SHARING)
              val member  = fieldIdentifierNode(ident, identifierName, identifierName)
              val callTpe = Some(registerType(tpe))
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
    val isInConstructor =
      Try(CPPVisitor.findEnclosingFunctionOrClass(fieldRef)).toOption.exists(_.isInstanceOf[ICPPConstructor])
    val op =
      if (fieldRef.isPointerDereference && !isInConstructor) Operators.indirectFieldAccess else Operators.fieldAccess
    val ma =
      callNode(fieldRef, code(fieldRef), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))
    val owner  = astForExpression(fieldRef.getFieldOwner)
    val member = fieldIdentifierNode(fieldRef, fieldRef.getFieldName.toString, fieldRef.getFieldName.toString)
    callAst(ma, List(owner, Ast(member)))
  }

  protected def astForArrayModifier(arrMod: IASTArrayModifier): Ast = {
    nullSafeAst(arrMod.getConstantExpression)
  }

  protected def astForQualifiedName(qualId: CPPASTQualifiedName): Ast = {
    safeGetBinding(qualId) match {
      case Some(function: ICPPFunction) =>
        val name      = qualId.getLastName.toString
        val signature = if (function.isExternC) "" else functionTypeToSignature(function.getType)
        val fullName = if (function.isExternC) {
          StringUtils.normalizeSpace(name)
        } else {
          val fullNameNoSig = StringUtils.normalizeSpace(function.getQualifiedName.mkString("."))
          s"$fullNameNoSig:$signature"
        }
        Ast(methodRefNode(qualId, name, fullName, registerType(function.getType.toString)))
      case _ =>
        val op = Operators.fieldAccess
        val ma =
          callNode(qualId, code(qualId), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))

        def fieldAccesses(names: List[IASTNode], argIndex: Int = -1): Ast = names match {
          case Nil => Ast()
          case head :: Nil =>
            astForNode(head)
          case head :: tail =>
            val codeString = s"${code(head)}::${tail.map(code).mkString("::")}"
            val callNode_ =
              callNode(head, code(head), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))
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
          replaceQualifiedNameSeparator(qualId.getLastName.toString),
          qualId.getLastName.toString
        )
        callAst(ma, List(owner, Ast(member)))
    }
  }

}
