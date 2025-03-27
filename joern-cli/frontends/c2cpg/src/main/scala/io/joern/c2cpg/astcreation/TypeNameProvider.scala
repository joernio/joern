package io.joern.c2cpg.astcreation

import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.core.dom.ast.gnu.c.ICASTKnRFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.*
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.nowarn
import scala.util.Try

trait TypeNameProvider { this: AstCreator =>

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val ReservedKeywordsAtTypes: List[String] =
    List(
      "const",
      "static",
      "restrict",
      "extern",
      "typedef",
      "inline",
      "constexpr",
      "auto",
      "virtual",
      "enum",
      "struct",
      "interface",
      "class"
    )

  private val KeywordsAtTypesToKeep: List[String] = List("unsigned", "volatile")

  protected def typeForDeclSpecifier(spec: IASTNode, index: Int = 0): String = {
    val tpeString = spec match {
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTFunctionDefinition] =>
        val parentDecl = s.getParent.asInstanceOf[IASTFunctionDefinition].getDeclarator
        ASTStringUtil.getReturnTypeString(s, parentDecl)
      case s: IASTSimpleDeclaration if s.getParent.isInstanceOf[ICASTKnRFunctionDeclarator] =>
        val decl = s.getDeclarators.toList(index)
        pointersAsString(s.getDeclSpecifier, decl)
      case s: IASTSimpleDeclSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTSimpleDeclSpecifier =>
        ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTNamedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTNamedTypeSpecifier =>
        ASTStringUtil.getSimpleName(s.getName)
      case s: IASTCompositeTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTCompositeTypeSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTEnumerationSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTEnumerationSpecifier => ASTStringUtil.getSimpleName(s.getName)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTParameterDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTParameterDeclaration].getDeclarator
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier if s.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        val parentDecl = s.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclarators.toList(index)
        pointersAsString(s, parentDecl)
      case s: IASTElaboratedTypeSpecifier => ASTStringUtil.getSignatureString(s, null)
      // TODO: handle other types of IASTDeclSpecifier
      case _ => Defines.Any
    }
    if (tpeString.isEmpty) Defines.Any else cleanType(tpeString)
  }

  protected def cleanType(rawType: String): String = {
    if (rawType == Defines.Any) return rawType
    val normalizedTpe = StringUtils.normalizeSpace(rawType.stripSuffix(" ()"))
    val tpe = ReservedKeywordsAtTypes.foldLeft(normalizedTpe) { (cur, repl) =>
      if (cur.startsWith(s"$repl ") || cur.contains(s" $repl ")) {
        cur.replace(s" $repl ", " ").stripPrefix(s"$repl ")
      } else cur
    }
    stripTemplateTags(replaceWhitespaceAfterKeyword(tpe)) match {
      // Empty or problematic types
      case ""                                                                      => Defines.Any
      case t if t.contains("?")                                                    => Defines.Any
      case t if t.contains("#")                                                    => Defines.Any
      case t if t.contains("::{") || t.contains("}::")                             => Defines.Any
      case t if (t.contains("{") || t.contains("}")) && !isThisLambdaCapture(t)    => Defines.Any
      case t if t.contains("org.eclipse.cdt.internal.core.dom.parser.ProblemType") => Defines.Any
      // Special patterns with specific handling
      case t if t.startsWith("[") && t.endsWith("]")       => Defines.Array
      case t if isThisLambdaCapture(t) || t.contains("->") => Defines.Function
      case t if t.contains("( ") => replaceQualifiedNameSeparator(t.substring(0, t.indexOf("( ")))
      // Default case
      case typeStr => replaceQualifiedNameSeparator(typeStr)
    }
  }

  protected def safeGetType(tpe: IType): String = {
    val tpeString = tpe match {
      case _: CPPClosureType                                      => Defines.Function
      case cppBasicType: ICPPBasicType if cppBasicType.isLong     => "long"
      case cppBasicType: ICPPBasicType if cppBasicType.isLongLong => "longlong"
      case cppBasicType: ICPPBasicType if cppBasicType.isShort    => "short"
      case _                                                      =>
        // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
        Try(ASTTypeUtil.getType(tpe)).getOrElse(Defines.Any)
    }
    cleanType(tpeString)
  }

  protected def functionTypeToSignature(typ: IFunctionType): String = {
    val returnType     = cleanType(safeGetType(typ.getReturnType))
    val parameterTypes = typ.getParameterTypes.map(t => cleanType(safeGetType(t)))
    StringUtils.normalizeSpace(s"$returnType(${parameterTypes.mkString(",")})")
  }

  @nowarn
  protected def typeFor(node: IASTNode): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    val tpeString = node match {
      case f: CPPASTFoldExpression                               => typeForCPPASTFoldExpression(f)
      case f: CPPASTFieldReference                               => typeForCPPASTFieldReference(f)
      case s: CPPASTIdExpression                                 => typeForCPPASTIdExpression(s)
      case s: ICPPASTNamedTypeSpecifier                          => typeForCPPAstNamedTypeSpecifier(s)
      case a: IASTArrayDeclarator                                => typeForIASTArrayDeclarator(a)
      case c: ICPPASTConstructorInitializer                      => typeForICPPASTConstructorInitializer(c)
      case c: CPPASTEqualsInitializer                            => typeForCPPASTEqualsInitializer(c)
      case _: IASTIdExpression | _: IASTName | _: IASTDeclarator => safeGetNodeType(node)
      case f: IASTFieldReference                                 => safeGetType(f.getFieldOwner.getExpressionType)
      case s: IASTNamedTypeSpecifier                             => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTCompositeTypeSpecifier                         => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTEnumerationSpecifier                           => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTElaboratedTypeSpecifier                        => ASTStringUtil.getReturnTypeString(s, null)
      case l: IASTLiteralExpression                              => safeGetType(l.getExpressionType)
      case e: IASTExpression                                     => safeGetNodeType(e)
      case d: IASTSimpleDeclaration                              => typeForDeclSpecifier(d.getDeclSpecifier)
      case _                                                     => getNodeSignature(node)
    }
    cleanType(tpeString)
  }

  private def replaceWhitespaceAfterKeyword(tpe: String): String = {
    if (KeywordsAtTypesToKeep.exists(k => tpe.startsWith(s"$k ") || tpe.contains(s" $k "))) {
      KeywordsAtTypesToKeep.foldLeft(tpe) { (cur, repl) =>
        val prefixStartsWith = s"$repl "
        val prefixContains   = s" $repl "
        if (cur.startsWith(prefixStartsWith)) {
          prefixStartsWith + replaceWhitespaceAfterKeyword(cur.substring(prefixStartsWith.length))
        } else if (cur.contains(prefixContains)) {
          val front = tpe.substring(0, tpe.indexOf(prefixContains))
          val back  = tpe.substring(tpe.indexOf(prefixContains) + prefixContains.length)
          s"${replaceWhitespaceAfterKeyword(front)}$prefixContains${replaceWhitespaceAfterKeyword(back)}"
        } else {
          cur
        }
      }
    } else {
      tpe.replace(" ", "")
    }
  }

  private def isThisLambdaCapture(tpe: String): Boolean = {
    tpe.startsWith("[*this]") || tpe.startsWith("[this]") || (tpe.startsWith("[") && tpe.contains("this]"))
  }

  private def pointersAsString(spec: IASTDeclSpecifier, parentDecl: IASTDeclarator): String = {
    val tpe = typeFor(spec) match {
      case Defines.Auto  => typeFor(parentDecl)
      case "longlongint" => "longlong"
      case "longint"     => "long"
      case "shortint"    => "short"
      case t             => t
    }
    val pointers = parentDecl.getPointerOperators
    val arr = parentDecl match {
      case p: IASTArrayDeclarator => p.getArrayModifiers.toList.map(_.getRawSignature).mkString
      case _                      => ""
    }
    if (pointers.isEmpty) {
      s"$tpe$arr"
    } else {
      val refs = pointers
        .map {
          case r: ICPPASTReferenceOperator if r.isRValueReference => "&&"
          case _: ICPPASTReferenceOperator                        => "&"
          case _: IASTPointer                                     => "*"
        }
        .mkString("")
      s"$tpe$arr$refs".strip()
    }
  }

  private def safeGetNodeType(node: IASTNode): String = {
    // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
    Try(ASTTypeUtil.getNodeType(node)).getOrElse(Defines.Any)
  }

  private def typeForCPPASTFieldReference(f: CPPASTFieldReference): String = {
    safeGetEvaluation(f.getFieldOwner) match {
      case Some(evaluation: EvalBinding) => evaluation.getType.toString
      case _                             => safeGetType(f.getFieldOwner.getExpressionType)
    }
  }

  private def typeForCPPASTFoldExpression(f: CPPASTFoldExpression): String = {
    safeGetEvaluation(f) match {
      case Some(evaluation: EvalFoldExpression) =>
        Try(evaluation.getValue.getEvaluation).toOption match {
          case Some(value: EvalBinary) =>
            val s = value.toString
            s.substring(0, s.indexOf(": "))
          case Some(value: EvalBinding) if value.getType.isInstanceOf[ICPPParameterPackType] =>
            value.getType.asInstanceOf[ICPPParameterPackType].getType.toString
          case _ => Defines.Any
        }
      case _ => Defines.Any
    }
  }

  @nowarn
  private def typeForIASTArrayDeclarator(a: IASTArrayDeclarator): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    if (safeGetNodeType(a).startsWith("? ")) {
      val tpe = getNodeSignature(a).replace("[]", "").strip()
      val arr = safeGetNodeType(a).replace("? ", "")
      s"$tpe$arr"
    } else if (safeGetNodeType(a).contains("} ") || safeGetNodeType(a).contains(" [")) {
      val tpe = getNodeSignature(a).replace("[]", "").strip()
      val arr = a.getArrayModifiers.map {
        case m if m.getConstantExpression != null => s"[${nodeSignature(m.getConstantExpression)}]"
        case _ if a.getInitializer != null =>
          a.getInitializer match {
            case l: IASTInitializerList => s"[${l.getSize}]"
            case _                      => "[]"
          }
        case _ => "[]"
      }.mkString
      s"$tpe$arr"
    } else {
      safeGetNodeType(a)
    }
  }

  private def typeForCPPASTIdExpression(s: CPPASTIdExpression): String = {
    safeGetEvaluation(s) match {
      case Some(evaluation: EvalMemberAccess) =>
        val deref = if (evaluation.isPointerDeref) "*" else ""
        evaluation.getOwnerType.toString + deref
      case Some(evalBinding: EvalBinding) =>
        evalBinding.getBinding match {
          case m: CPPMethod   => safeGetNodeType(m.getPrimaryDeclaration)
          case f: CPPFunction => safeGetNodeType(f.getDefinition)
          case v: CPPVariable => v.getType.toString
          case _              => safeGetNodeType(s)
        }
      case _ => safeGetNodeType(s)
    }
  }

  @nowarn
  private def typeForICPPASTConstructorInitializer(c: ICPPASTConstructorInitializer): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    c.getParent match {
      case initializer: ICPPASTConstructorChainInitializer => fullName(initializer.getMemberInitializerId)
      case _                                               => getNodeSignature(c)
    }
  }

  @nowarn
  private def typeForCPPASTEqualsInitializer(c: CPPASTEqualsInitializer): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    c.getInitializerClause match {
      case initializer: ICPPASTFunctionCallExpression
          if initializer.getFunctionNameExpression.isInstanceOf[CPPASTIdExpression] =>
        val name = initializer.getFunctionNameExpression.asInstanceOf[CPPASTIdExpression]
        typeForCPPASTIdExpression(name)
      case _ =>
        getNodeSignature(c)
    }
  }

  private def typeForCPPAstNamedTypeSpecifier(s: ICPPASTNamedTypeSpecifier): String = {
    safeGetBinding(s) match {
      case Some(spec: ICPPSpecialization) => spec.toString
      case Some(n: ICPPBinding)           => n.getQualifiedName.mkString(".")
      case Some(other: IBinding)          => other.toString
      case _ if s.getName != null         => ASTStringUtil.getQualifiedName(s.getName)
      case _                              => s.getRawSignature
    }
  }

}
