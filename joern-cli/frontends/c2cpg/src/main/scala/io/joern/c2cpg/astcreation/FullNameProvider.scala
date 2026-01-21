package io.joern.c2cpg.astcreation

import io.joern.x2cpg.Defines as X2CpgDefines
import io.joern.x2cpg.passes.frontend.MetaDataPass
import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.c.{CASTFunctionDeclarator, CVariable}
import org.eclipse.cdt.internal.core.dom.parser.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinding
import org.eclipse.cdt.internal.core.model.ASTStringUtil

import scala.annotation.tailrec
import scala.util.Try

object FullNameProvider {

  type MethodLike = IASTFunctionDeclarator | IASTFunctionDefinition | ICPPASTLambdaExpression

  private val TagsToKeepInFullName = List(
    "<anonymous>",
    "<const>",
    "<duplicate>",
    "<enum>",
    "<extension>",
    "<global>",
    "<iterator>",
    "<lambda>",
    "<param>",
    "<tmp>",
    "<type>"
  )

  /** Removes template type parameters from qualified names while preserving special tags.
    *
    * This method strips the angle brackets and their contents from type names, which is useful for simplifying complex
    * templated type names. It preserves certain special tags that are enclosed in angle brackets (like &lt;lambda&gt;,
    * &lt;const&gt;, etc.) to maintain semantic meaning.
    *
    * Examples:
    * {{{
    *  stripTemplateTags("std::vector<int>") == "std::vector"
    *  stripTemplateTags("Foo.Bar<T>.<lambda>1") == "Foo.Bar.<lambda>1" // preserves the special <lambda> tag
    *  stripTemplateTags("std::map<std::string, std::vector<int>>") == "std::map" // removes nested template parameters
    * }}}
    *
    * @param input
    *   The input string that may contain template tags
    * @return
    *   The string with template tags removed but special tags preserved
    */
  def stripTemplateTags(input: String): String = {
    if (input.isEmpty || !input.contains("<") || !input.contains(">")) {
      return input
    }

    val firstOpenIndex = input.indexOf("<")
    // Find matching closing bracket, accounting for nesting
    var nesting    = 1
    var closeIndex = firstOpenIndex + 1
    while (closeIndex < input.length && nesting > 0) {
      if (input(closeIndex) == '<') nesting += 1
      else if (input(closeIndex) == '>') nesting -= 1
      closeIndex += 1
    }
    closeIndex -= 1 // Adjust to point at the closing bracket

    val prefix = input.substring(0, firstOpenIndex)
    val tag    = input.substring(firstOpenIndex, closeIndex + 1)
    val suffix = input.substring(closeIndex + 1)

    // Keep special tags, remove others
    if (TagsToKeepInFullName.contains(tag)) {
      s"$prefix$tag${stripTemplateTags(suffix)}"
    } else {
      s"$prefix${stripTemplateTags(suffix)}"
    }
  }

  final case class MethodFullNameInfo(name: String, fullName: String, signature: String, returnType: String)

  final case class TypeFullNameInfo(name: String, fullName: String)
}

trait FullNameProvider { this: AstCreator =>

  import FullNameProvider.*

  protected def replaceQualifiedNameSeparator(name: String): String = {
    if (name.isEmpty || name == Defines.Any || name == Defines.Void) return name
    val normalizedName = StringUtils.normalizeSpace(name)
    normalizedName.replace(Defines.QualifiedNameSeparator, ".").stripPrefix(".")
  }

  protected def methodFullNameInfo(methodLike: MethodLike): MethodFullNameInfo = {
    val name_             = shortName(methodLike)
    val fullName_         = fullName(methodLike)
    val returnType_       = returnType(methodLike)
    val signature_        = signature(returnType_, methodLike)
    val sanitizedFullName = sanitizeMethodLikeFullName(name_, fullName_, signature_, methodLike)
    MethodFullNameInfo(name_, sanitizedFullName, signature_, returnType_)
  }

  protected def shortName(node: IASTNode): String = {
    val name = node match {
      case n: IASTName                        => shortNameForIASTName(n)
      case d: IASTDeclarator                  => shortNameForIASTDeclarator(d)
      case f: IASTFunctionDefinition          => shortNameForIASTFunctionDefinition(f)
      case d: CPPASTIdExpression              => shortNameForCPPASTIdExpression(d)
      case u: IASTUnaryExpression             => shortName(u.getOperand)
      case c: IASTFunctionCallExpression      => shortName(c.getFunctionNameExpression)
      case d: IASTIdExpression                => shortName(d.getName)
      case m: IASTPreprocessorMacroDefinition => shortName(m.getName)
      case n: ICPPASTNamespaceDefinition      => shortName(n.getName)
      case e: IASTEnumerationSpecifier        => shortName(e.getName)
      case c: IASTCompositeTypeSpecifier      => shortName(c.getName)
      case e: IASTElaboratedTypeSpecifier     => shortName(e.getName)
      case s: IASTNamedTypeSpecifier          => shortName(s.getName)
      case l: IASTLabelStatement              => shortName(l.getName)
      case s: IASTSimpleDeclSpecifier         => s.getRawSignature
      case _: ICPPASTLambdaExpression         => nextClosureName()
      case other                              => notHandledYet(other); nextClosureName()
    }
    stripTemplateTags(replaceOperator(StringUtils.normalizeSpace(name)))
  }

  protected def fullName(node: IASTNode): String = {
    fullNameFromBinding(node) match {
      case Some(fullName) =>
        StringUtils.normalizeSpace(fullName)
      case None =>
        val qualifiedName = node match {
          case _: IASTTranslationUnit                            => ""
          case aliasDecl: ICPPASTAliasDeclaration                => fullNameForICPPASTAliasDeclaration(aliasDecl)
          case namespace: ICPPASTNamespaceDefinition             => fullNameForICPPASTNamespaceDefinition(namespace)
          case compType: IASTCompositeTypeSpecifier              => fullNameForIASTCompositeTypeSpecifier(compType)
          case enumSpecifier: IASTEnumerationSpecifier           => fullNameForIASTEnumerationSpecifier(enumSpecifier)
          case namedType: IASTNamedTypeSpecifier                 => fullNameForIASTNamedTypeSpecifier(namedType)
          case f: IASTFunctionDeclarator                         => fullNameForIASTFunctionDeclarator(f)
          case f: IASTFunctionDefinition                         => fullNameForIASTFunctionDefinition(f)
          case e: IASTElaboratedTypeSpecifier                    => fullNameForIASTElaboratedTypeSpecifier(e)
          case _: ICPPASTLambdaExpression                        => fullNameForICPPASTLambdaExpression()
          case qfn: ICPPASTQualifiedName                         => fullNameForICPPASTQualifiedName(qfn)
          case id: IASTIdExpression                              => fullNameForIASTIdExpression(id)
          case decl: IASTDeclarator                              => fullNameForIASTDeclarator(decl)
          case u: IASTUnaryExpression                            => code(u.getOperand)
          case other if other != null && other.getParent != null => fullName(other.getParent)
          case other if other != null                            => notHandledYet(other); ""
          case null                                              => ""
        }
        stripTemplateTags(replaceQualifiedNameSeparator(qualifiedName).stripPrefix("."))
    }
  }

  protected def signature(returnType: String, methodLike: MethodLike): String = {
    val constFlag = if (isConst(methodLike)) { Defines.ConstSuffix }
    else { "" }
    StringUtils.normalizeSpace(s"$returnType${parameterListSignature(methodLike)}$constFlag")
  }

  private def shortNameForIASTName(node: IASTName): String = {
    node match {
      case c: ICPPASTConversionName =>
        val name = replaceOperator(ASTStringUtil.getSimpleName(c))
        if (name.contains(Defines.QualifiedNameSeparator)) {
          name.split(Defines.QualifiedNameSeparator).last
        } else {
          name
        }
      case otherName => ASTStringUtil.getSimpleName(otherName)
    }
  }

  private def fullNameForIASTDeclarator(decl: IASTDeclarator): String = {
    Try(ASTStringUtil.getQualifiedName(decl.getName)).getOrElse(nextClosureName())
  }

  private def fullNameForIASTIdExpression(id: IASTIdExpression): String = {
    Try(ASTStringUtil.getQualifiedName(id.getName)).getOrElse(nextClosureName())
  }

  private def fullNameForICPPASTQualifiedName(qfn: ICPPASTQualifiedName): String = {
    Try(ASTStringUtil.getQualifiedName(qfn)).getOrElse(nextClosureName())
  }

  private def fullNameForICPPASTAliasDeclaration(alias: ICPPASTAliasDeclaration): String = {
    ASTStringUtil.getQualifiedName(alias.getAlias)
  }

  private def fullNameForICPPASTLambdaExpression(): String = {
    val globalFullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))
    val fullName = scope.computeScopePath match {
      case ""    => ""
      case other => s".$other"
    }
    s"$globalFullName$fullName"
  }

  private def isCPPFunction(methodLike: MethodLike): Boolean = {
    methodLike.isInstanceOf[CPPASTFunctionDeclarator] || methodLike.isInstanceOf[CPPASTFunctionDefinition]
  }

  private def sanitizeMethodLikeFullName(
    name: String,
    fullName: String,
    signature: String,
    methodLike: MethodLike
  ): String = {
    fullName match {
      case f if methodLike.isInstanceOf[ICPPASTLambdaExpression] && (f.contains("[") || f.contains("{")) =>
        s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature"
      case f if methodLike.isInstanceOf[ICPPASTLambdaExpression] && f.isEmpty =>
        s"$name:$signature"
      case f if methodLike.isInstanceOf[ICPPASTLambdaExpression] =>
        s"$f.$name:$signature"
      case f if isCPPFunction(methodLike) && (f.isEmpty || f == s"${X2CpgDefines.UnresolvedNamespace}.") =>
        s"${X2CpgDefines.UnresolvedNamespace}.$name:$signature"
      case f if isCPPFunction(methodLike) && f.contains("?") =>
        s"${StringUtils.normalizeSpace(f).takeWhile(_ != ':')}:$signature"
      case f if f.isEmpty || f == s"${X2CpgDefines.UnresolvedNamespace}." =>
        s"${X2CpgDefines.UnresolvedNamespace}.$name"
      case other if other.nonEmpty => other
      case _                       => s"${X2CpgDefines.UnresolvedNamespace}.$name"
    }
  }

  private def parameterListSignature(func: IASTNode): String = {
    val parameter = parameters(func)
    val elements = parameter.map {
      case p: IASTParameterDeclaration => typeForDeclSpecifier(p.getDeclSpecifier)
      case other                       => typeForDeclSpecifier(other)
    }
    val variadicString = variadicStringForFunction(func, parameter)
    s"(${elements.mkString(",")}$variadicString)"
  }

  private def shortNameForIASTDeclarator(declarator: IASTDeclarator): String = {
    safeGetBinding(declarator.getName).map(_.getName).getOrElse {
      if (
        (declarator.getName == null || ASTStringUtil.getSimpleName(declarator.getName).isEmpty)
        && declarator.getNestedDeclarator != null
      ) { shortName(declarator.getNestedDeclarator) }
      else { ASTStringUtil.getSimpleName(declarator.getName) }
    }
  }

  private def shortNameForIASTFunctionDefinition(definition: IASTFunctionDefinition): String = {
    if (
      ASTStringUtil.getSimpleName(definition.getDeclarator.getName).isEmpty
      && definition.getDeclarator.getNestedDeclarator != null
    ) {
      shortName(definition.getDeclarator.getNestedDeclarator)
    } else {
      shortName(definition.getDeclarator.getName)
    }
  }

  private def shortNameForCPPASTIdExpression(d: CPPASTIdExpression): String = {
    safeGetEvaluation(d) match {
      case Some(evalBinding: EvalBinding) =>
        evalBinding.getBinding match {
          case f: CPPFunction if f.getDeclarations != null =>
            f.getDeclarations.headOption.map(shortName).getOrElse(f.getName)
          case f: CPPFunction if f.getDefinition != null =>
            shortName(f.getDefinition)
          case other =>
            other.getName
        }
      case _ => shortName(d.getName)
    }
  }

  private def replaceOperator(name: String): String = {
    name
      .replace("operator class ", "")
      .replace("operator enum ", "")
      .replace("operator struct ", "")
      .replace("operator ", "")
  }

  @tailrec
  private def isConst(node: IASTNode): Boolean = {
    node match {
      case lambdaExpression: ICPPASTLambdaExpression => isConst(lambdaExpression.getDeclarator)
      case definition: ICPPASTFunctionDefinition     => isConst(definition.getDeclarator)
      case declarator: ICPPASTFunctionDeclarator     => declarator.isConst
      case _                                         => false
    }
  }

  protected def bindsToConstructor(funcDef: IASTFunctionDefinition): Boolean = {
    safeGetBinding(funcDef.getDeclarator.getName) match {
      case Some(_: ICPPConstructor) => true
      case _                        => bindsToConstructor(funcDef.getDeclarator.getName)
    }

  }

  protected def bindsToConstructor(name: IASTName): Boolean = {
    val fullName_ = ASTStringUtil.getQualifiedName(name)
    val name_     = ASTStringUtil.getSimpleName(name)
    scope.computeScopePath.endsWith(s".$name_")
    || (scope.computeScopePath.nonEmpty && scope.computeScopePath == name_)
    || fullName_.startsWith(s"$name_::$name_")
  }

  private def fullNameFromBinding(node: IASTNode): Option[String] = {
    node match {
      case id: CPPASTIdExpression =>
        safeGetEvaluation(id) match {
          case Some(evalBinding: EvalBinding) =>
            evalBinding.getBinding match {
              case f: CPPFunction if f.getDeclarations != null =>
                Option(f.getDeclarations.headOption.map(n => s"${fullName(n)}").getOrElse(f.getName))
              case f: CPPFunction if f.getDefinition != null =>
                Option(s"${fullName(f.getDefinition)}")
              case other =>
                Option(other.getName)
            }
          case _ => None
        }
      case declarator: CPPASTFunctionDeclarator =>
        safeGetBinding(declarator.getName) match {
          case Some(function: ICPPFunction) if declarator.getName.isInstanceOf[ICPPASTConversionName] =>
            val tpe        = cleanType(typeFor(declarator.getName.asInstanceOf[ICPPASTConversionName].getTypeId))
            val returnType = cleanType(safeGetType(function.getType.getReturnType))
            val fullNameNoSig = replaceQualifiedNameSeparator(
              function.getQualifiedName.takeWhile(!_.startsWith("operator ")).mkString(".")
            )
            val fn = if (function.isExternC) { tpe }
            else { s"${stripTemplateTags(fullNameNoSig)}.$tpe:${signature(returnType, declarator)}" }
            Option(fn)
          case Some(constructor: ICPPConstructor) =>
            val fullNameNoSig = replaceQualifiedNameSeparator(
              replaceOperator(constructor.getQualifiedName.mkString("."))
            )
            val fn = if (constructor.isExternC) {
              replaceOperator(constructor.getName)
            } else {
              val sig = signature(Defines.Void, declarator)
              s"${stripTemplateTags(fullNameNoSig)}:$sig"
            }
            Option(fn)
          case Some(function: ICPPFunction) =>
            val fullNameNoSig = replaceQualifiedNameSeparator(replaceOperator(function.getQualifiedName.mkString(".")))
            val fn = if (function.isExternC) {
              replaceOperator(function.getName)
            } else {
              val returnTpe = declarator.getParent match {
                case definition: ICPPASTFunctionDefinition if !bindsToConstructor(definition) => returnType(definition)
                case _ => safeGetType(function.getType.getReturnType)
              }
              val sig = signature(cleanType(returnTpe), declarator)
              s"${stripTemplateTags(fullNameNoSig)}:$sig"
            }
            Option(fn)
          case Some(x @ (_: ICPPField | _: CPPVariable)) =>
            val fullNameNoSig = replaceQualifiedNameSeparator(x.getQualifiedName.mkString("."))
            val fn = if (x.isExternC) { x.getName }
            else { s"${stripTemplateTags(fullNameNoSig)}:${cleanType(safeGetType(x.getType))}" }
            Option(fn)
          case Some(_: IProblemBinding) if bindsToConstructor(declarator.getName) =>
            val fullNameNoSig = replaceQualifiedNameSeparator(
              replaceOperator(ASTStringUtil.getQualifiedName(declarator.getName))
            )
            val sig = signature(Defines.Void, declarator)
            val fn  = s"${stripTemplateTags(fullNameNoSig)}:$sig"
            Option(fn)
          case Some(_: IProblemBinding) =>
            val fullNameNoSig = replaceOperator(ASTStringUtil.getQualifiedName(declarator.getName))
            val fixedFullName = replaceQualifiedNameSeparator(fullNameNoSig)
            val returnTpe = declarator.getParent match {
              case definition: ICPPASTFunctionDefinition if !bindsToConstructor(definition) => returnType(definition)
              case _                                                                        => returnType(declarator)
            }
            val signature_ = signature(returnTpe, declarator)
            if (fixedFullName.isEmpty) { Option(s"${X2CpgDefines.UnresolvedNamespace}:$signature_") }
            else { Option(s"${stripTemplateTags(fixedFullName)}:$signature_") }
          case _ => None
        }
      case declarator: CASTFunctionDeclarator =>
        safeGetBinding(declarator.getName) match {
          case Some(cVariable: CVariable)     => Option(cVariable.getName)
          case Some(cppVariable: CPPVariable) => Option(cppVariable.getName)
          case _                              => Option(declarator.getName.toString)
        }
      case definition: ICPPASTFunctionDefinition =>
        Some(fullName(definition.getDeclarator))
      case typeSpecifier: CPPASTNamedTypeSpecifier if typeSpecifier.isFriend =>
        safeGetBinding(typeSpecifier.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(s"${scope.computeScopePath}.${b.getName}")
          case _                                          => None
        }
      case typeSpecifier: IASTNamedTypeSpecifier =>
        safeGetBinding(typeSpecifier.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case namespace: ICPPASTNamespaceDefinition =>
        safeGetBinding(namespace.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case compType: IASTCompositeTypeSpecifier =>
        safeGetBinding(compType.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case enumSpecifier: IASTEnumerationSpecifier =>
        safeGetBinding(enumSpecifier.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case e: IASTElaboratedTypeSpecifier =>
        safeGetBinding(e.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case e: IASTDeclarator =>
        safeGetBinding(e.getName) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case e: ICPPASTAliasDeclaration =>
        safeGetBinding(e.getAlias) match {
          case Some(b: ICPPBinding) if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                          => None
        }
      case _ => None
    }
  }

  private def fullNameForICPPASTNamespaceDefinition(namespace: ICPPASTNamespaceDefinition): String = {
    val name = shortName(namespace)
    s"${scope.computeScopePath}.$name"
  }

  private def fullNameForIASTCompositeTypeSpecifier(compositeTypeSpecifier: IASTCompositeTypeSpecifier): String = {
    val name = shortName(compositeTypeSpecifier)
    if (name.nonEmpty) {
      s"${scope.computeScopePath}.$name"
    } else {
      val name = compositeTypeSpecifier.getParent match {
        case decl: IASTSimpleDeclaration =>
          decl.getDeclarators.headOption
            .map(shortName)
            .getOrElse(scopeLocalUniqueName("type"))
        case _ => scopeLocalUniqueName("type")
      }
      s"${scope.computeScopePath}.$name"
    }
  }

  private def fullNameForIASTEnumerationSpecifier(enumerationSpecifier: IASTEnumerationSpecifier): String = {
    val name = shortName(enumerationSpecifier)
    s"${scope.computeScopePath}.$name"
  }

  private def fullNameForIASTElaboratedTypeSpecifier(elaboratedTypeSpecifier: IASTElaboratedTypeSpecifier): String = {
    val name = shortName(elaboratedTypeSpecifier)
    s"${scope.computeScopePath}.$name"
  }

  private def fullNameForIASTFunctionDeclarator(functionDeclarator: IASTFunctionDeclarator): String = {
    Try(ASTStringUtil.getQualifiedName(functionDeclarator.getName)).getOrElse(nextClosureName())
  }

  private def fullNameForIASTFunctionDefinition(functionDefinition: IASTFunctionDefinition): String = {
    Try(ASTStringUtil.getQualifiedName(functionDefinition.getDeclarator.getName)).getOrElse(nextClosureName())
  }

  private def fullNameForIASTNamedTypeSpecifier(typeSpecifier: IASTNamedTypeSpecifier): String = {
    typeSpecifier match {
      case typeSpecifier: CPPASTNamedTypeSpecifier if typeSpecifier.isFriend =>
        s"${scope.computeScopePath}.${typeSpecifier.getName}"
      case _ =>
        Try(ASTStringUtil.getQualifiedName(typeSpecifier.getName)).getOrElse(shortName(typeSpecifier))
    }

  }

}
