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

object TypeNameProvider {

  private type TypeLike = IASTEnumerationSpecifier | ICPPASTNamespaceDefinition | IASTCompositeTypeSpecifier |
    IASTElaboratedTypeSpecifier

}

trait TypeNameProvider { this: AstCreator =>

  import FullNameProvider.*
  import TypeNameProvider.TypeLike

  // Sadly, there is no predefined List / Enum of this within Eclipse CDT:
  private val ReservedKeywordsAtTypes: List[String] =
    List(
      "auto",
      "class",
      "const",
      "constexpr",
      "enum",
      "extern",
      "inline",
      "interface",
      "restrict",
      "static",
      "struct",
      "typedef",
      "virtual"
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
      case cppBasicType: ICPPBasicType if cppBasicType.isLong     => Defines.Long
      case cppBasicType: ICPPBasicType if cppBasicType.isLongLong => Defines.LongLong
      case cppBasicType: ICPPBasicType if cppBasicType.isShort    => Defines.Short
      case templateType: ICPPTemplateTypeParameter                => templateType.getName
      case cppPackType: ICPPParameterPackType =>
        cppPackType.getType match {
          case templateType: ICPPTemplateTypeParameter                 => templateType.getName
          case refType: ICPPReferenceType if refType.isRValueReference => safeGetType(refType.getType) + "&&"
          case refType: ICPPReferenceType                              => safeGetType(refType.getType)
          case other => Try(ASTTypeUtil.getType(other)).getOrElse(Defines.Any)
        }
      case _ =>
        // In case of unresolved includes etc. this may fail throwing an unrecoverable exception
        Try(ASTTypeUtil.getType(tpe)).getOrElse(Defines.Any)
    }
    cleanType(tpeString)
  }

  protected def variadicStringForFunction(
    func: IFunctionType | IASTNode,
    parameter: Seq[IASTNode] = Seq.empty
  ): String = {
    val takesVarArgs = func match {
      case tpe: IFunctionType => tpe.takesVarArgs()
      case func: IASTNode     => isVariadic(func) || parameter.lastOption.exists(paramIsVariadic)
    }
    // See: https://en.cppreference.com/w/cpp/language/variadic_arguments
    // `...` and `,...` are the same but `...` is deprecated since C++26 so we settle for the newer one
    if (takesVarArgs) { ",..." }
    else { "" }

  }

  protected def functionInstanceToSignature(function: ICPPFunctionInstance, tpe: IFunctionType): String = {
    Try(function.getSpecializedBinding).toOption match {
      case Some(binding: ICPPFunctionTemplate) =>
        val returnType     = cleanType(safeGetType(tpe.getReturnType))
        val parameterTypes = binding.getParameters.map(t => cleanType(safeGetType(t.getType)))
        val variadicString = variadicStringForFunction(tpe)
        StringUtils.normalizeSpace(s"$returnType(${parameterTypes.mkString(",")}$variadicString)")
      case _ => functionTypeToSignature(tpe)
    }
  }

  protected def functionTypeToSignature(tpe: IFunctionType): String = {
    val returnType     = cleanType(safeGetType(tpe.getReturnType))
    val parameterTypes = tpe.getParameterTypes.map(t => cleanType(safeGetType(t)))
    val variadicString = variadicStringForFunction(tpe)
    StringUtils.normalizeSpace(s"$returnType(${parameterTypes.mkString(",")}$variadicString)")
  }

  protected def typeFullNameInfo(typeLike: TypeLike): TypeFullNameInfo = {
    typeLike match {
      case _: IASTElaboratedTypeSpecifier =>
        val name_     = shortName(typeLike)
        val fullName_ = registerType(cleanType(fullName(typeLike)))
        TypeFullNameInfo(name_, fullName_)
      case e: IASTEnumerationSpecifier =>
        val name_                            = shortName(e)
        val fullName_                        = fullName(e)
        val (uniqueName, uniqueNameFullName) = scopeLocalUniqueName(name_, fullName_, "enum")
        TypeFullNameInfo(uniqueName, uniqueNameFullName)
      case n: ICPPASTNamespaceDefinition =>
        val name = shortName(n) match {
          case ""    => "<anonymous>"
          case other => other
        }
        val fullName_ = fullName(n)
        TypeFullNameInfo(name, scopeLocalUniqueNamespaceFullName(fullName_))
      case s: IASTCompositeTypeSpecifier =>
        val fullName_ = registerType(cleanType(fullName(s)))
        val name_ = shortName(s) match {
          case n if n.isEmpty && fullName_.contains(".") => fullName_.substring(fullName_.lastIndexOf("."))
          case n if n.isEmpty                            => fullName_
          case other                                     => other
        }
        TypeFullNameInfo(name_, fullName_)
    }
  }

  private val FundamentalTypeKeywords = List(
    "void",
    "bool",
    "char",
    "char8_t",
    "char16_t",
    "char32_t",
    "wchar_t",
    "int",
    "short",
    "long",
    "signed",
    "unsigned",
    "float",
    "double"
  )

  /** https://en.cppreference.com/w/cpp/language/types */
  protected def isFundamentalTypeKeywords(tpe: String): Boolean =
    FundamentalTypeKeywords.contains(tpe.replace("*", "").replace("&", ""))

  /** https://www.w3schools.com/cpp/cpp_variables_identifiers.asp */
  protected def isValidFullNameChar(char: Char): Boolean = char match {
    case c if c.isLetterOrDigit => true
    case c if c == '_'          => true
    case c if c == '.'          => true
    case _                      => false
  }

  @nowarn
  protected def typeFor(node: IASTNode): String = {
    import org.eclipse.cdt.core.dom.ast.ASTSignatureUtil.getNodeSignature
    val tpeString = node match {
      case f: CPPASTFoldExpression          => typeForCPPASTFoldExpression(f)
      case f: CPPASTFieldReference          => typeForCPPASTFieldReference(f)
      case s: CPPASTIdExpression            => typeForCPPASTIdExpression(s)
      case s: ICPPASTNamedTypeSpecifier     => typeForCPPAstNamedTypeSpecifier(s)
      case a: IASTArrayDeclarator           => typeForIASTArrayDeclarator(a)
      case c: ICPPASTConstructorInitializer => typeForICPPASTConstructorInitializer(c)
      case c: CPPASTEqualsInitializer       => typeForCPPASTEqualsInitializer(c)
      case n: IASTName                      => typeForIASTName(n)
      case id: IASTIdExpression             => typeForIASTName(id.getName)
      case d: IASTDeclarator                => typeForIASTName(d.getName)
      case f: IASTFieldReference            => safeGetType(f.getFieldOwner.getExpressionType)
      case s: IASTNamedTypeSpecifier        => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTCompositeTypeSpecifier    => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTEnumerationSpecifier      => ASTStringUtil.getReturnTypeString(s, null)
      case s: IASTElaboratedTypeSpecifier   => ASTStringUtil.getReturnTypeString(s, null)
      case l: IASTLiteralExpression         => safeGetType(l.getExpressionType)
      case e: IASTExpression                => safeGetNodeType(e)
      case d: IASTSimpleDeclaration         => typeForDeclSpecifier(d.getDeclSpecifier)
      case _                                => getNodeSignature(node)
    }
    cleanType(tpeString)
  }

  private def typeForIASTName(name: IASTName): String = {
    val x = safeGetNodeType(name)
    safeGetBinding(name) match {
      case Some(v: IVariable) =>
        v.getType match {
          case f: IFunctionType =>
            f.getReturnType.toString
          case c: ICPPBinding =>
            c.getQualifiedName.mkString(".")
          case other => other.toString
        }
      case _ => safeGetNodeType(name)
    }
  }

  protected def returnType(methodLike: FullNameProvider.MethodLike): String = {
    methodLike match {
      case declarator: IASTFunctionDeclarator => returnTypeForIASTFunctionDeclarator(declarator)
      case definition: IASTFunctionDefinition => returnTypeForIASTFunctionDefinition(definition)
      case lambda: ICPPASTLambdaExpression    => returnTypeForICPPASTLambdaExpression(lambda)
    }
  }

  private def returnTypeForIASTFunctionDeclarator(declarator: IASTFunctionDeclarator): String = {
    safeGetBinding(declarator.getName) match {
      case Some(_: ICPPConstructor) => Defines.Void
      case Some(_: ICPPFunctionTemplate) if declarator.getParent.isInstanceOf[IASTFunctionDefinition] =>
        cleanType(typeForDeclSpecifier(declarator.getParent.asInstanceOf[IASTFunctionDefinition].getDeclSpecifier))
      case Some(value: ICPPMethod) if !value.getType.toString.startsWith("?") =>
        cleanType(safeGetType(value.getType.getReturnType))
      case Some(value: ICPPFunction) if !value.getType.toString.startsWith("?") =>
        cleanType(safeGetType(value.getType.getReturnType))
      case _ if declarator.getParent.isInstanceOf[IASTSimpleDeclaration] =>
        cleanType(typeForDeclSpecifier(declarator.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier))
      case _ if declarator.getParent.isInstanceOf[IASTFunctionDefinition] =>
        cleanType(typeForDeclSpecifier(declarator.getParent.asInstanceOf[IASTFunctionDefinition].getDeclSpecifier))
      case Some(_: IProblemBinding) if bindsToConstructor(declarator.getName) => Defines.Void
      case _                                                                  => Defines.Any
    }
  }

  private def returnTypeForIASTFunctionDefinition(definition: IASTFunctionDefinition): String = {
    returnTypeForIASTFunctionDeclarator(definition.getDeclarator)
  }

  private def returnTypeForICPPASTLambdaExpression(lambda: ICPPASTLambdaExpression): String = {
    lambda.getDeclarator match {
      case declarator: IASTDeclarator if declarator.getTrailingReturnType != null =>
        typeForDeclSpecifier(declarator.getTrailingReturnType.getDeclSpecifier)
      case _ =>
        safeGetEvaluation(lambda) match {
          case Some(value) if !value.toString.endsWith(": <unknown>") => cleanType(value.getType.toString)
          case Some(value) if value.getType.isInstanceOf[CPPClosureType] =>
            val closureType = value.getType.asInstanceOf[CPPClosureType]
            closureType.getMethods
              .find(_.toString.startsWith("operator ()"))
              .map(_.getType)
              .collect {
                case t: ICPPFunctionType if t.getReturnType.isInstanceOf[CPPClosureType] => Defines.Function
                case t: ICPPFunctionType => cleanType(safeGetType(t.getReturnType))
              }
              .getOrElse(Defines.Any)
          case _ => Defines.Any
        }
    }
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
      case "longlongint" => Defines.LongLong
      case "longint"     => Defines.Long
      case "shortint"    => Defines.Short
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
          case v: CPPVariable => safeGetNodeType(v.getDefinition)
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
