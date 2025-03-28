package io.joern.c2cpg.astcreation

import org.apache.commons.lang3.StringUtils
import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.core.dom.ast.cpp.*
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTIdExpression
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.EvalBinding
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFunctionDefinition
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPFunction
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPVariable
import org.eclipse.cdt.internal.core.model.ASTStringUtil
import io.joern.x2cpg.Defines as X2CpgDefines
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CVariable
import org.eclipse.cdt.internal.core.dom.parser.cpp.CPPClosureType

import scala.annotation.tailrec
import scala.util.Try

object FullNameProvider {
  private type MethodLike = IASTFunctionDeclarator | IASTFunctionDefinition | ICPPASTLambdaExpression

  private type TypeLike = IASTEnumerationSpecifier | ICPPASTNamespaceDefinition | ICPPASTNamespaceAlias |
    IASTCompositeTypeSpecifier | IASTElaboratedTypeSpecifier

  // Data structure to track segments and their positions during stripTemplateTags
  private case class Segment(text: String, start: Int, isTag: Boolean)

  private val TagsToKeepInFullName = List(
    "<anonymous>",
    "<iterator>",
    "<lambda>",
    "<global>",
    "<param>",
    "<const>",
    "<alias>",
    "<type>",
    "<enum>",
    "<tmp>"
    // Sort tags by length (descending) to handle overlapping tags correctly in case there are any
  ).sortBy(-_.length)

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
    if (input.isEmpty || !input.contains("<")) {
      return input
    }

    // Start with the entire string as one non-tag segment
    var segments = List(Segment(input, 0, false))

    // For each tag, split any matching segments further
    for (tag <- TagsToKeepInFullName) {
      segments = segments.flatMap { segment =>
        if (segment.isTag) {
          // Don't split tags
          List(segment)
        } else {
          // Find tag positions in this segment
          val segmentText = segment.text
          val matches     = tag.r.findAllMatchIn(segmentText).toList

          if (matches.isEmpty) {
            // No matches in this segment
            List(segment)
          } else {
            // Split segment at tag positions
            var result  = List[Segment]()
            var lastEnd = 0

            for (m <- matches) {
              // Add text before tag
              if (m.start > lastEnd) {
                result = result :+ Segment(segmentText.substring(lastEnd, m.start), segment.start + lastEnd, false)
              }

              // Add tag
              result = result :+ Segment(tag, segment.start + m.start, true)
              lastEnd = m.end
            }

            // Add remaining text after last tag
            if (lastEnd < segmentText.length) {
              result = result :+ Segment(segmentText.substring(lastEnd), segment.start + lastEnd, false)
            }

            result
          }
        }
      }
    }

    // Apply template tag removal to non-tag segments only
    segments.map { segment =>
      if (segment.isTag) {
        segment.text
      } else {
        val firstIndex = segment.text.indexOf("<")
        val lastIndex  = segment.text.lastIndexOf(">")
        if (firstIndex != -1 && lastIndex != -1 && firstIndex < lastIndex) {
          val prefix = segment.text.substring(0, firstIndex)
          val suffix = segment.text.substring(lastIndex + 1)
          prefix + suffix
        } else {
          segment.text
        }
      }
    }.mkString
  }

}

trait FullNameProvider { this: AstCreator =>

  import FullNameProvider.*

  protected def replaceQualifiedNameSeparator(name: String): String = {
    if (name.isEmpty) return name
    val normalizedName = StringUtils.normalizeSpace(name)
    normalizedName
      .stripPrefix(Defines.QualifiedNameSeparator)
      .replace(Defines.QualifiedNameSeparator, ".")
      .stripPrefix(".")
  }

  protected def methodFullNameInfo(methodLike: MethodLike): MethodFullNameInfo = {
    val name_             = shortName(methodLike)
    val fullName_         = fullName(methodLike)
    val returnType_       = returnType(methodLike)
    val signature_        = signature(returnType_, methodLike)
    val sanitizedFullName = sanitizeMethodLikeFullName(name_, fullName_, signature_, methodLike)
    MethodFullNameInfo(name_, sanitizedFullName, signature_, returnType_)
  }

  protected def typeFullNameInfo(typeLike: TypeLike): TypeFullNameInfo = {
    typeLike match {
      case _: IASTElaboratedTypeSpecifier =>
        val name_     = shortName(typeLike)
        val fullName_ = registerType(cleanType(fullName(typeLike)))
        TypeFullNameInfo(name_, fullName_)
      case e: IASTEnumerationSpecifier =>
        val name_                              = shortName(e)
        val fullName_                          = fullName(e)
        val (uniqueName_, uniqueNameFullName_) = fileLocalUniqueName(name_, fullName_, "enum")
        TypeFullNameInfo(uniqueName_, uniqueNameFullName_)
      case n: ICPPASTNamespaceDefinition =>
        val name_                              = shortName(n)
        val fullName_                          = fullName(n)
        val (uniqueName_, uniqueNameFullName_) = fileLocalUniqueName(name_, fullName_, "namespace")
        TypeFullNameInfo(uniqueName_, uniqueNameFullName_)
      case a: ICPPASTNamespaceAlias =>
        val name_     = shortName(a)
        val fullName_ = fullName(a)
        TypeFullNameInfo(name_, fullName_)
      case s: IASTCompositeTypeSpecifier =>
        val fullName_ = registerType(cleanType(fullName(s)))
        val name_ = shortName(s) match {
          case n if n.isEmpty => lastNameOfQualifiedName(fullName_)
          case other          => other
        }
        TypeFullNameInfo(name_, fullName_)
    }
  }

  protected def shortName(node: IASTNode): String = {
    val name = node match {
      case s: IASTSimpleDeclSpecifier     => s.getRawSignature
      case d: IASTDeclarator              => shortNameForIASTDeclarator(d)
      case f: ICPPASTFunctionDefinition   => shortNameForICPPASTFunctionDefinition(f)
      case f: IASTFunctionDefinition      => shortNameForIASTFunctionDefinition(f)
      case u: IASTUnaryExpression         => shortName(u.getOperand)
      case c: IASTFunctionCallExpression  => shortName(c.getFunctionNameExpression)
      case d: CPPASTIdExpression          => shortNameForCPPASTIdExpression(d)
      case d: IASTIdExpression            => shortNameForIASTIdExpression(d)
      case a: ICPPASTNamespaceAlias       => ASTStringUtil.getSimpleName(a.getAlias)
      case n: ICPPASTNamespaceDefinition  => ASTStringUtil.getSimpleName(n.getName)
      case e: IASTEnumerationSpecifier    => ASTStringUtil.getSimpleName(e.getName)
      case c: IASTCompositeTypeSpecifier  => ASTStringUtil.getSimpleName(c.getName)
      case e: IASTElaboratedTypeSpecifier => ASTStringUtil.getSimpleName(e.getName)
      case s: IASTNamedTypeSpecifier      => ASTStringUtil.getSimpleName(s.getName)
      case _: ICPPASTLambdaExpression     => nextClosureName()
      case other =>
        notHandledYet(other)
        nextClosureName()
    }
    stripTemplateTags(StringUtils.normalizeSpace(name))
  }

  protected def fullName(node: IASTNode): String = {
    fullNameFromBinding(node) match {
      case Some(fullName) =>
        StringUtils.normalizeSpace(fullName)
      case None =>
        val qualifiedName = node match {
          case _: IASTTranslationUnit => ""
          case alias: ICPPASTNamespaceAlias =>
            replaceQualifiedNameSeparator(ASTStringUtil.getQualifiedName(alias.getMappingName))
          case namespace: ICPPASTNamespaceDefinition   => fullNameForICPPASTNamespaceDefinition(namespace)
          case compType: IASTCompositeTypeSpecifier    => fullNameForIASTCompositeTypeSpecifier(compType)
          case enumSpecifier: IASTEnumerationSpecifier => fullNameForIASTEnumerationSpecifier(enumSpecifier)
          case namedType: IASTNamedTypeSpecifier       => fullNameForIASTNamedTypeSpecifier(namedType)
          case f: IASTFunctionDeclarator               => fullNameForIASTFunctionDeclarator(f)
          case f: IASTFunctionDefinition               => fullNameForIASTFunctionDefinition(f)
          case e: IASTElaboratedTypeSpecifier          => fullNameForIASTElaboratedTypeSpecifier(e)
          case d: IASTIdExpression                     => ASTStringUtil.getSimpleName(d.getName)
          case u: IASTUnaryExpression                  => code(u.getOperand)
          case x: ICPPASTQualifiedName    => replaceQualifiedNameSeparator(ASTStringUtil.getQualifiedName(x))
          case _: ICPPASTLambdaExpression => fullNameForICPPASTLambdaExpression()
          case other if other != null && other.getParent != null => fullName(other.getParent)
          case other if other != null                            => notHandledYet(other); ""
          case null                                              => ""
        }
        stripTemplateTags(replaceQualifiedNameSeparator(qualifiedName).stripPrefix("."))
    }
  }

  protected def returnType(methodLike: MethodLike): String = {
    methodLike match {
      case declarator: IASTFunctionDeclarator => returnTypeForIASTFunctionDeclarator(declarator)
      case definition: IASTFunctionDefinition => returnTypeForIASTFunctionDefinition(definition)
      case lambda: ICPPASTLambdaExpression    => returnTypeForICPPASTLambdaExpression(lambda)
    }
  }

  protected def signature(returnType: String, methodLike: MethodLike): String = {
    val constFlag = if (isConst(methodLike)) { Defines.ConstSuffix }
    else { "" }
    StringUtils.normalizeSpace(s"$returnType${parameterListSignature(methodLike)}$constFlag")
  }

  private def lastNameOfQualifiedName(name: String): String = {
    val normalizedName = StringUtils.normalizeSpace(replaceOperator(name))
    val cleanedName    = normalizedName.takeWhile(_ != '<')
    cleanedName.split(Defines.QualifiedNameSeparator).lastOption.getOrElse(cleanedName)
  }

  private def fullNameForICPPASTLambdaExpression(): String = {
    methodAstParentStack
      .collectFirst {
        case t: NewTypeDecl =>
          if (t.name != NamespaceTraversal.globalNamespaceName) {
            val globalFullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))
            s"$globalFullName.${t.fullName}"
          } else {
            t.fullName
          }
        case m: NewMethod =>
          val fullNameWithoutSignature = m.fullName.stripSuffix(s":${m.signature}")
          if (!m.name.startsWith("<lambda>") && m.name != NamespaceTraversal.globalNamespaceName) {
            val globalFullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))
            s"$globalFullName.$fullNameWithoutSignature"
          } else {
            fullNameWithoutSignature
          }
      }
      .mkString("")
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

  private def returnTypeForIASTFunctionDeclarator(declarator: IASTFunctionDeclarator): String = {
    safeGetBinding(declarator.getName) match {
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
      case _ => Defines.Any
    }
  }

  private def returnTypeForIASTFunctionDefinition(definition: IASTFunctionDefinition): String = {
    if (isCppConstructor(definition)) {
      cleanType(typeFor(definition.asInstanceOf[CPPASTFunctionDefinition].getMemberInitializers.head.getInitializer))
    } else {
      safeGetBinding(definition.getDeclarator.getName) match {
        case Some(_: ICPPFunctionTemplate) =>
          typeForDeclSpecifier(definition.getDeclSpecifier)
        case Some(value: ICPPMethod) if !value.getType.toString.startsWith("?") =>
          cleanType(safeGetType(value.getType.getReturnType))
        case Some(value: ICPPFunction) if !value.getType.toString.startsWith("?") =>
          cleanType(safeGetType(value.getType.getReturnType))
        case _ =>
          typeForDeclSpecifier(definition.getDeclSpecifier)
      }
    }
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

  private def parameterListSignature(func: IASTNode): String = {
    val variadic = if (isVariadic(func)) "..." else ""
    val elements = parameters(func).map {
      case p: IASTParameterDeclaration => typeForDeclSpecifier(p.getDeclSpecifier)
      case other                       => typeForDeclSpecifier(other)
    }
    s"(${elements.mkString(",")}$variadic)"
  }

  private def shortNameForIASTDeclarator(declarator: IASTDeclarator): String = {
    safeGetBinding(declarator.getName).map(_.getName).getOrElse {
      if (ASTStringUtil.getSimpleName(declarator.getName).isEmpty && declarator.getNestedDeclarator != null) {
        shortName(declarator.getNestedDeclarator)
      } else {
        ASTStringUtil.getSimpleName(declarator.getName)
      }
    }
  }

  private def shortNameForICPPASTFunctionDefinition(definition: ICPPASTFunctionDefinition): String = {
    if (
      ASTStringUtil.getSimpleName(definition.getDeclarator.getName).isEmpty
      && definition.getDeclarator.getNestedDeclarator != null
    ) {
      shortName(definition.getDeclarator.getNestedDeclarator)
    } else {
      lastNameOfQualifiedName(ASTStringUtil.getSimpleName(definition.getDeclarator.getName))
    }
  }

  private def shortNameForIASTFunctionDefinition(definition: IASTFunctionDefinition): String = {
    if (
      ASTStringUtil.getSimpleName(definition.getDeclarator.getName).isEmpty
      && definition.getDeclarator.getNestedDeclarator != null
    ) {
      shortName(definition.getDeclarator.getNestedDeclarator)
    } else {
      ASTStringUtil.getSimpleName(definition.getDeclarator.getName)
    }
  }

  private def shortNameForCPPASTIdExpression(d: CPPASTIdExpression): String = {
    val name = safeGetEvaluation(d) match {
      case Some(evalBinding: EvalBinding) =>
        evalBinding.getBinding match {
          case f: CPPFunction if f.getDeclarations != null =>
            f.getDeclarations.headOption.map(n => ASTStringUtil.getSimpleName(n.getName)).getOrElse(f.getName)
          case f: CPPFunction if f.getDefinition != null => ASTStringUtil.getSimpleName(f.getDefinition.getName)
          case other                                     => other.getName
        }
      case _ => ASTStringUtil.getSimpleName(d.getName)
    }
    lastNameOfQualifiedName(name)
  }

  private def shortNameForIASTIdExpression(d: IASTIdExpression): String = {
    lastNameOfQualifiedName(ASTStringUtil.getSimpleName(d.getName))
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
          case Some(function: ICPPFunction) =>
            val fullNameNoSig = replaceQualifiedNameSeparator(replaceOperator(function.getQualifiedName.mkString(".")))
            val fn = if (function.isExternC) {
              replaceOperator(function.getName)
            } else {
              val returnTpe = declarator.getParent match {
                case definition: ICPPASTFunctionDefinition if !isCppConstructor(definition) => returnType(definition)
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
          case Some(_: IProblemBinding) =>
            val fullNameNoSig = replaceOperator(ASTStringUtil.getQualifiedName(declarator.getName))
            val fixedFullName = replaceQualifiedNameSeparator(fullNameNoSig)
            val returnTpe = declarator.getParent match {
              case definition: ICPPASTFunctionDefinition if !isCppConstructor(definition) => returnType(definition)
              case _                                                                      => returnType(declarator)
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
      case _ => None
    }
  }

  private def fullNameForICPPASTNamespaceDefinition(namespace: ICPPASTNamespaceDefinition): String = {
    s"${fullName(namespace.getParent)}.${ASTStringUtil.getSimpleName(namespace.getName)}"
  }

  private def fullNameForIASTCompositeTypeSpecifier(compType: IASTCompositeTypeSpecifier): String = {
    if (ASTStringUtil.getSimpleName(compType.getName).nonEmpty) {
      s"${fullName(compType.getParent)}.${ASTStringUtil.getSimpleName(compType.getName)}"
    } else {
      val name = compType.getParent match {
        case decl: IASTSimpleDeclaration =>
          decl.getDeclarators.headOption
            .map(n => ASTStringUtil.getSimpleName(n.getName))
            .getOrElse(fileLocalUniqueName("", "", "type")._1)
        case _ => fileLocalUniqueName("", "", "type")._1
      }
      s"${fullName(compType.getParent)}.$name"
    }
  }

  private def fullNameForIASTEnumerationSpecifier(enumSpecifier: IASTEnumerationSpecifier): String = {
    s"${fullName(enumSpecifier.getParent)}.${ASTStringUtil.getSimpleName(enumSpecifier.getName)}"
  }

  private def fullNameForIASTNamedTypeSpecifier(typeSpecifier: IASTNamedTypeSpecifier): String = {
    s"${fullName(typeSpecifier.getParent)}.${ASTStringUtil.getSimpleName(typeSpecifier.getName)}"
  }

  private def fullNameForIASTElaboratedTypeSpecifier(e: IASTElaboratedTypeSpecifier): String = {
    s"${fullName(e.getParent)}.${ASTStringUtil.getSimpleName(e.getName)}"
  }

  private def fullNameForIASTFunctionDeclarator(f: IASTFunctionDeclarator): String = {
    Try(replaceQualifiedNameSeparator(ASTStringUtil.getQualifiedName(f.getName))).getOrElse(nextClosureName())
  }

  private def fullNameForIASTFunctionDefinition(f: IASTFunctionDefinition): String = {
    Try(replaceQualifiedNameSeparator(ASTStringUtil.getQualifiedName(f.getDeclarator.getName)))
      .getOrElse(nextClosureName())
  }

  protected final case class MethodFullNameInfo(name: String, fullName: String, signature: String, returnType: String)

  protected final case class TypeFullNameInfo(name: String, fullName: String)

}
