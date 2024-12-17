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
import org.eclipse.cdt.internal.core.dom.parser.c.CASTFunctionDeclarator
import org.eclipse.cdt.internal.core.dom.parser.c.CVariable

import scala.util.Try

trait FullNameProvider { this: AstCreator =>

  protected type MethodLike = IASTFunctionDeclarator | IASTFunctionDefinition | ICPPASTLambdaExpression

  protected type TypeLike = IASTEnumerationSpecifier | ICPPASTNamespaceDefinition | ICPPASTNamespaceAlias |
    IASTCompositeTypeSpecifier | IASTElaboratedTypeSpecifier

  protected def fixQualifiedName(name: String): String = {
    if (name.isEmpty) { name }
    else {
      val normalizedName = StringUtils.normalizeSpace(name)
      normalizedName
        .stripPrefix(Defines.QualifiedNameSeparator)
        .replace(Defines.QualifiedNameSeparator, ".")
        .stripPrefix(".")
    }
  }

  protected def isQualifiedName(name: String): Boolean =
    name.startsWith(Defines.QualifiedNameSeparator)

  protected def lastNameOfQualifiedName(name: String): String = {
    val normalizedName = StringUtils.normalizeSpace(replaceOperator(name))
    val cleanedName = if (normalizedName.contains("<") && normalizedName.contains(">")) {
      name.substring(0, normalizedName.indexOf("<"))
    } else {
      normalizedName
    }
    cleanedName.split(Defines.QualifiedNameSeparator).lastOption.getOrElse(cleanedName)
  }

  protected def methodFullNameInfo(methodLike: MethodLike): MethodFullNameInfo = {
    val returnType_       = returnType(methodLike)
    val signature_        = signature(returnType_, methodLike)
    val name_             = shortName(methodLike)
    val fullName_         = fullName(methodLike)
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
        val (uniqueName_, uniqueNameFullName_) = uniqueName("enum", name_, fullName_)
        TypeFullNameInfo(uniqueName_, uniqueNameFullName_)
      case n: ICPPASTNamespaceDefinition =>
        val name_                              = shortName(n)
        val fullName_                          = fullName(n)
        val (uniqueName_, uniqueNameFullName_) = uniqueName("namespace", name_, fullName_)
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
    StringUtils.normalizeSpace(name)
  }

  protected def fullName(node: IASTNode): String = {
    fullNameFromBinding(node) match {
      case Some(fullName) =>
        StringUtils.normalizeSpace(fullName)
      case None =>
        val qualifiedName = node match {
          case _: IASTTranslationUnit       => ""
          case alias: ICPPASTNamespaceAlias => fixQualifiedName(ASTStringUtil.getQualifiedName(alias.getMappingName))
          case namespace: ICPPASTNamespaceDefinition             => fullNameForICPPASTNamespaceDefinition(namespace)
          case compType: IASTCompositeTypeSpecifier              => fullNameForIASTCompositeTypeSpecifier(compType)
          case enumSpecifier: IASTEnumerationSpecifier           => fullNameForIASTEnumerationSpecifier(enumSpecifier)
          case f: IASTFunctionDeclarator                         => fullNameForIASTFunctionDeclarator(f)
          case f: IASTFunctionDefinition                         => fullNameForIASTFunctionDefinition(f)
          case e: IASTElaboratedTypeSpecifier                    => fullNameForIASTElaboratedTypeSpecifier(e)
          case d: IASTIdExpression                               => ASTStringUtil.getSimpleName(d.getName)
          case u: IASTUnaryExpression                            => code(u.getOperand)
          case x: ICPPASTQualifiedName                           => fixQualifiedName(ASTStringUtil.getQualifiedName(x))
          case other if other != null && other.getParent != null => fullName(other.getParent)
          case other if other != null                            => notHandledYet(other); ""
          case null                                              => ""
        }
        fixQualifiedName(qualifiedName).stripPrefix(".")
    }
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
        s"${X2CpgDefines.UnresolvedNamespace}.$name"
      case f if methodLike.isInstanceOf[ICPPASTLambdaExpression] && f.isEmpty =>
        name
      case f if methodLike.isInstanceOf[ICPPASTLambdaExpression] =>
        s"$f.$name"
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
    cleanType(typeForDeclSpecifier(declarator.getParent.asInstanceOf[IASTSimpleDeclaration].getDeclSpecifier))
  }

  private def returnTypeForIASTFunctionDefinition(definition: IASTFunctionDefinition): String = {
    if (isCppConstructor(definition)) {
      typeFor(definition.asInstanceOf[CPPASTFunctionDefinition].getMemberInitializers.head.getInitializer)
    } else {
      typeForDeclSpecifier(definition.getDeclSpecifier)
    }
  }

  private def returnTypeForICPPASTLambdaExpression(lambda: ICPPASTLambdaExpression): String = {
    lambda.getDeclarator match {
      case declarator: IASTDeclarator =>
        Option(declarator.getTrailingReturnType)
          .map(id => typeForDeclSpecifier(id.getDeclSpecifier))
          .getOrElse(Defines.Any)
      case null => Defines.Any
    }
  }

  private def returnType(methodLike: MethodLike): String = {
    methodLike match {
      case declarator: IASTFunctionDeclarator => returnTypeForIASTFunctionDeclarator(declarator)
      case definition: IASTFunctionDefinition => returnTypeForIASTFunctionDefinition(definition)
      case lambda: ICPPASTLambdaExpression    => returnTypeForICPPASTLambdaExpression(lambda)
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

  private def signature(returnType: String, methodLike: MethodLike): String = {
    StringUtils.normalizeSpace(s"$returnType${parameterListSignature(methodLike)}")
  }

  private def shortNameForIASTDeclarator(declarator: IASTDeclarator): String = {
    Try(declarator.getName.resolveBinding().getName).getOrElse {
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
        declarator.getName.resolveBinding() match {
          case function: ICPPFunction if declarator.getName.isInstanceOf[ICPPASTConversionName] =>
            val tpe = typeFor(declarator.getName.asInstanceOf[ICPPASTConversionName].getTypeId)
            val fullNameNoSig = fixQualifiedName(
              function.getQualifiedName.takeWhile(!_.startsWith("operator ")).mkString(".")
            )
            val fn = if (function.isExternC) {
              tpe
            } else {
              s"$fullNameNoSig.$tpe:${functionTypeToSignature(function.getType)}"
            }
            Option(fn)
          case function: ICPPFunction =>
            val fullNameNoSig = fixQualifiedName(replaceOperator(function.getQualifiedName.mkString(".")))
            val fn = if (function.isExternC) {
              replaceOperator(function.getName)
            } else {
              s"$fullNameNoSig:${functionTypeToSignature(function.getType)}"
            }
            Option(fn)
          case x @ (_: ICPPField | _: CPPVariable) =>
            val fullNameNoSig = fixQualifiedName(x.getQualifiedName.mkString("."))
            val fn = if (x.isExternC) {
              x.getName
            } else {
              s"$fullNameNoSig:${cleanType(safeGetType(x.getType))}"
            }
            Option(fn)
          case _: IProblemBinding =>
            val fullNameNoSig = replaceOperator(ASTStringUtil.getQualifiedName(declarator.getName))
            val fixedFullName = fixQualifiedName(fullNameNoSig)
            if (fixedFullName.isEmpty) {
              Option(s"${X2CpgDefines.UnresolvedNamespace}:${X2CpgDefines.UnresolvedSignature}")
            } else {
              Option(s"$fixedFullName:${X2CpgDefines.UnresolvedSignature}")
            }
          case _ => None
        }
      case declarator: CASTFunctionDeclarator =>
        declarator.getName.resolveBinding() match {
          case cVariable: CVariable => Option(cVariable.getName)
          case _                    => Option(declarator.getName.toString)
        }
      case definition: ICPPASTFunctionDefinition =>
        Some(fullName(definition.getDeclarator))
      case namespace: ICPPASTNamespaceDefinition =>
        namespace.getName.resolveBinding() match {
          case b: ICPPBinding if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                    => None
        }
      case compType: IASTCompositeTypeSpecifier =>
        compType.getName.resolveBinding() match {
          case b: ICPPBinding if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                    => None
        }
      case enumSpecifier: IASTEnumerationSpecifier =>
        enumSpecifier.getName.resolveBinding() match {
          case b: ICPPBinding if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                    => None
        }
      case e: IASTElaboratedTypeSpecifier =>
        e.getName.resolveBinding() match {
          case b: ICPPBinding if b.getName.nonEmpty => Option(b.getQualifiedName.mkString("."))
          case _                                    => None
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
            .getOrElse(uniqueName("composite_type", "", "")._1)
        case _ => uniqueName("composite_type", "", "")._1
      }
      s"${fullName(compType.getParent)}.$name"
    }
  }

  private def fullNameForIASTEnumerationSpecifier(enumSpecifier: IASTEnumerationSpecifier): String = {
    s"${fullName(enumSpecifier.getParent)}.${ASTStringUtil.getSimpleName(enumSpecifier.getName)}"
  }

  private def fullNameForIASTElaboratedTypeSpecifier(e: IASTElaboratedTypeSpecifier): String = {
    s"${fullName(e.getParent)}.${ASTStringUtil.getSimpleName(e.getName)}"
  }

  private def fullNameForIASTFunctionDeclarator(f: IASTFunctionDeclarator): String = {
    Try(fixQualifiedName(ASTStringUtil.getQualifiedName(f.getName))).getOrElse(nextClosureName())
  }

  private def fullNameForIASTFunctionDefinition(f: IASTFunctionDefinition): String = {
    Try(fixQualifiedName(ASTStringUtil.getQualifiedName(f.getDeclarator.getName))).getOrElse(nextClosureName())
  }

  protected final case class MethodFullNameInfo(name: String, fullName: String, signature: String, returnType: String)

  protected final case class TypeFullNameInfo(name: String, fullName: String)

}
