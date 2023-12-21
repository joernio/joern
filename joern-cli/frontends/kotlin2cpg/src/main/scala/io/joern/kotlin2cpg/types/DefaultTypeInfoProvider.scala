package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.psi.PsiUtils
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.passes.KeyPool
import org.jetbrains.kotlin.cli.jvm.compiler.{
  KotlinCoreEnvironment,
  KotlinToJVMBytecodeCompiler,
  NoScopeRecordCliBindingTrace
}
import org.jetbrains.kotlin.com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.{
  DeclarationDescriptor,
  DescriptorVisibility,
  FunctionDescriptor,
  ValueDescriptor,
  ValueParameterDescriptor
}
import kotlin.reflect.jvm.internal.impl.load.java.descriptors.JavaClassConstructorDescriptor
import org.jetbrains.kotlin.descriptors.impl.{
  ClassConstructorDescriptorImpl,
  EnumEntrySyntheticClassDescriptor,
  LazyPackageViewDescriptorImpl,
  PropertyDescriptorImpl,
  TypeAliasConstructorDescriptorImpl
}
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaClassDescriptor
import org.jetbrains.kotlin.load.java.sources.JavaSourceElement
import org.jetbrains.kotlin.load.java.structure.impl.classFiles.BinaryJavaMethod
import org.jetbrains.kotlin.psi.{
  KtAnnotationEntry,
  KtArrayAccessExpression,
  KtBinaryExpression,
  KtCallExpression,
  KtClassBody,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtExpression,
  KtFile,
  KtLambdaExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtParameter,
  KtPrimaryConstructor,
  KtProperty,
  KtPsiUtil,
  KtQualifiedExpression,
  KtSecondaryConstructor,
  KtSuperExpression,
  KtThisExpression,
  KtTypeAlias,
  KtTypeReference
}
import org.jetbrains.kotlin.resolve.{BindingContext, DescriptorToSourceUtils, DescriptorUtils}
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor
import org.jetbrains.kotlin.types.TypeUtils
import org.jetbrains.kotlin.types.error.ErrorType
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class DefaultTypeInfoProvider(environment: KotlinCoreEnvironment) extends TypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  import DefaultTypeInfoProvider._

  val bindingContext: BindingContext = {
    Try {
      logger.info("Running Kotlin compiler analysis...")
      val t0             = System.currentTimeMillis()
      val analysisResult = KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      val t1             = System.currentTimeMillis()
      logger.info(s"Kotlin compiler analysis finished in `${t1 - t0}` ms.")
      analysisResult
    } match {
      case Success(analysisResult) => analysisResult.getBindingContext
      case Failure(exc) =>
        logger.error(s"Kotlin compiler analysis failed with exception `${exc.toString}`:`${exc.getMessage}`.")
        BindingContext.EMPTY
    }
  }

  def isValidRender(render: String): Boolean = {
    !render.contains("ERROR")
  }

  def anySignature(args: Seq[Any]): String = {
    val argsSignature =
      if (args.isEmpty) ""
      else if (args.size == 1) TypeConstants.any
      else s"${TypeConstants.any}${s",${TypeConstants.any}" * (args.size - 1)}"
    s"${TypeConstants.any}($argsSignature)"
  }

  def usedAsExpression(expr: KtExpression): Option[Boolean] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.USED_AS_EXPRESSION.getKey)).map(_.booleanValue())
  }

  def fullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(TypeRenderer.renderFqNameForDesc)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def visibility(fn: KtNamedFunction): Option[DescriptorVisibility] = {
    val mapForEntity = bindingsForEntity(bindingContext, fn)
    Option(mapForEntity.get(BindingContext.FUNCTION.getKey))
      .map(_.getVisibility)
  }

  def containingTypeDeclFullName(ktFn: KtNamedFunction, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, ktFn)
    Option(mapForEntity.get(BindingContext.FUNCTION.getKey))
      .map { fnDesc =>
        if (DescriptorUtils.isExtension(fnDesc))
          TypeRenderer.render(fnDesc.getExtensionReceiverParameter.getType)
        else
          TypeRenderer.renderFqNameForDesc(fnDesc.getContainingDeclaration)
      }
      .getOrElse(defaultValue)
  }

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean = {
    resolvedCallDescriptor(expr)
      .map(_.getSource)
      .collect { case s: JavaSourceElement => s }
      .map(_.getJavaElement)
      .collect { case bjm: BinaryJavaMethod => bjm }
      .exists(_.isStatic)
  }

  def fullNameWithSignature(expr: KtDestructuringDeclarationEntry, defaultValue: (String, String)): (String, String) = {
    Option(bindingContext.get(BindingContext.COMPONENT_RESOLVED_CALL, expr))
      .map { resolvedCall =>
        val fnDesc = resolvedCall.getResultingDescriptor
        val relevantDesc =
          if (!fnDesc.isActual && fnDesc.getOverriddenDescriptors.asScala.nonEmpty)
            fnDesc.getOverriddenDescriptors.asScala.toList.head
          else fnDesc
        val renderedFqName     = TypeRenderer.renderFqNameForDesc(relevantDesc)
        val returnTypeFullName = renderedReturnType(relevantDesc.getOriginal)

        val renderedParameterTypes =
          relevantDesc.getValueParameters.asScala.toSeq
            .map(renderTypeForParameterDesc(_))
            .mkString(",")
        val signature = s"$returnTypeFullName($renderedParameterTypes)"
        val fullName  = s"$renderedFqName:$signature"

        if (!isValidRender(fullName) || !isValidRender(signature)) defaultValue
        else (fullName, signature)
      }
      .getOrElse(defaultValue)
  }

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity)
      .map(_.getKeys)
      .exists(_.contains(BindingContext.SHORT_REFERENCE_TO_COMPANION_OBJECT.getKey))
  }

  def typeFullName(expr: KtDestructuringDeclarationEntry, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.VARIABLE.getKey))
      .map { desc => TypeRenderer.render(desc.getType) }
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtTypeReference, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE.getKey))
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtCallExpression, defaultValue: String): String = {
    resolvedCallDescriptor(expr)
      .map(_.getOriginal)
      .map { originalDesc =>
        val relevantDesc =
          if (!originalDesc.isActual && originalDesc.getOverriddenDescriptors.asScala.nonEmpty)
            originalDesc.getOverriddenDescriptors.asScala.toList.head
          else originalDesc
        if (isConstructorCall(expr).getOrElse(false)) TypeConstants.void
        else renderedReturnType(relevantDesc.getOriginal)
      }
      .getOrElse(defaultValue)
  }

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(_.getExpandedType)
      .filterNot(_.isInstanceOf[ErrorType])
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def returnType(expr: KtNamedFunction, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.FUNCTION, expr))
      .map(_.getReturnType)
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def propertyType(expr: KtProperty, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.VARIABLE.getKey))
      .map(_.getType)
      .filterNot(_.isInstanceOf[ErrorType])
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(
        Option(expr.getTypeReference)
          .map { typeRef =>
            typeFromImports(typeRef.getText, expr.getContainingKtFile).getOrElse(typeRef.getText)
          }
          .getOrElse(defaultValue)
      )
  }

  def typeFullName(expr: KtClassOrObject, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(_.getDefaultType)
      .map(TypeRenderer.render(_))
      .getOrElse(defaultValue)
  }

  def inheritanceTypes(expr: KtClassOrObject, defaultValue: Seq[String]): Seq[String] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(getSuperclassDescriptors)
      .filter(_.asScala.nonEmpty)
      .map(
        _.asScala
          .map { superClassDesc =>
            TypeRenderer.render(superClassDesc.getDefaultType)
          }
          .toList
      )
      .getOrElse(defaultValue)
  }

  def anonymousObjectIdx(obj: KtElement): Option[Int] = {
    val parentFn      = KtPsiUtil.getTopmostParentOfTypes(obj, classOf[KtNamedFunction])
    val containingObj = Option(parentFn).getOrElse(obj.getContainingKtFile)
    PsiUtils.objectIdxMaybe(obj, containingObj)
  }

  def fullName(
    expr: KtClassOrObject,
    defaultValue: String,
    anonymousCtxMaybe: Option[AnonymousObjectContext] = None
  ): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    val nonLocalFullName = Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(_.getDefaultType)
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)

    if (anonymousCtxMaybe.nonEmpty) {
      anonymousCtxMaybe
        .map { _ =>
          val fnDescMaybe = Option(mapForEntity.get(BindingContext.CLASS.getKey))
          fnDescMaybe
            .map(_.getContainingDeclaration)
            .map { containingDecl =>
              val idxMaybe = anonymousObjectIdx(expr)
              val idx      = idxMaybe.map(_.toString).getOrElse("nan")
              s"${TypeRenderer.renderFqNameForDesc(containingDecl.getOriginal).stripSuffix(".")}" + "$object$" + s"$idx"
            }
            .getOrElse(nonLocalFullName)
        }
        .getOrElse(nonLocalFullName)
    } else if (expr.isLocal) {
      val fnDescMaybe = Option(mapForEntity.get(BindingContext.CLASS.getKey))
      fnDescMaybe
        .map(_.getContainingDeclaration)
        .map { containingDecl =>
          s"${TypeRenderer.renderFqNameForDesc(containingDecl.getOriginal)}.${expr.getName}"
        }
        .getOrElse(nonLocalFullName)
    } else nonLocalFullName
  }

  def isCompanionObject(expr: KtClassOrObject): Boolean = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey)).exists(DescriptorUtils.isCompanionObject(_))
  }

  def typeFullName(expr: KtParameter, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.VALUE_PARAMETER.getKey))
      .map(_.getType)
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def expressionType(expr: KtExpression, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .flatMap(tpeInfo => Option(tpeInfo.getType))
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def fullNameWithSignature(expr: KtClassLiteralExpression, defaultValue: (String, String)): (String, String) = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .map(_.getType)
      .map(_.getArguments.asScala)
      .filter(_.nonEmpty)
      .map { typeArguments =>
        val firstTypeArg = typeArguments.toList.head
        val rendered     = TypeRenderer.render(firstTypeArg.getType)
        val retType      = expressionType(expr, TypeConstants.any)
        val signature    = s"$retType()"
        val fullName     = s"$rendered.${TypeConstants.classLiteralReplacementMethodName}:$signature"
        (fullName, signature)
      }
      .getOrElse(defaultValue)
  }

  private def subexpressionForResolvedCallInfo(expr: KtExpression): KtExpression = {
    expr match {
      case typedExpr: KtCallExpression =>
        Option(typedExpr.getFirstChild)
          .collect { case expr: KtExpression => expr }
          .getOrElse(expr)
      case typedExpr: KtQualifiedExpression =>
        Option(typedExpr.getSelectorExpression)
          .collect { case expr: KtCallExpression => expr }
          .map(subexpressionForResolvedCallInfo)
          .getOrElse(typedExpr)
      case typedExpr: KtBinaryExpression =>
        Option(typedExpr.getChildren.toList(1))
          .collect { case expr: KtExpression => expr }
          .getOrElse(expr)
      case _ => expr
    }
  }

  private def resolvedCallDescriptor(expr: KtExpression): Option[FunctionDescriptor] = {
    val relevantSubexpression = subexpressionForResolvedCallInfo(expr)
    val descMaybe = for {
      callForSubexpression         <- Option(bindingContext.get(BindingContext.CALL, relevantSubexpression))
      resolvedCallForSubexpression <- Option(bindingContext.get(BindingContext.RESOLVED_CALL, callForSubexpression))
      desc = resolvedCallForSubexpression.getResultingDescriptor
    } yield desc

    descMaybe.collect { case desc: FunctionDescriptor => desc }
  }

  def isConstructorDescriptor(desc: FunctionDescriptor): Boolean = {
    desc match {
      case _: JavaClassConstructorDescriptor     => true
      case _: ClassConstructorDescriptorImpl     => true
      case _: TypeAliasConstructorDescriptorImpl => true
      case _                                     => false
    }
  }

  def isConstructorCall(expr: KtExpression): Option[Boolean] = {
    expr match {
      case _: KtCallExpression | _: KtQualifiedExpression =>
        resolvedCallDescriptor(expr) match {
          case Some(desc) if isConstructorDescriptor(desc) => Some(true)
          case _                                           => Some(false)
        }
      case _ => Some(false)
    }
  }

  def fullNameWithSignature(expr: KtCallExpression, defaultValue: (String, String)): (String, String) = {
    resolvedCallDescriptor(expr) match {
      case Some(desc) =>
        val originalDesc = desc.getOriginal
        val relevantDesc = originalDesc match {
          case typedDesc: TypeAliasConstructorDescriptorImpl =>
            typedDesc.getUnderlyingConstructorDescriptor
          case typedDesc: FunctionDescriptor if !typedDesc.isActual =>
            val overwriddenDescriptors = typedDesc.getOverriddenDescriptors.asScala.toList
            if (overwriddenDescriptors.nonEmpty) overwriddenDescriptors.head
            else typedDesc
          case _ => originalDesc
        }
        val returnTypeFullName =
          if (isConstructorCall(expr).getOrElse(false)) TypeConstants.void
          else renderedReturnType(relevantDesc.getOriginal)
        val renderedParameterTypes =
          relevantDesc.getValueParameters.asScala.toSeq
            .map(renderTypeForParameterDesc(_))
            .mkString(",")
        val signature = s"$returnTypeFullName($renderedParameterTypes)"

        val renderedFqName = TypeRenderer.renderFqNameForDesc(relevantDesc)
        val fullName =
          if (isConstructorCall(expr).getOrElse(false)) s"$renderedFqName${TypeConstants.initPrefix}:$signature"
          else s"$renderedFqName:$signature"
        if (!isValidRender(fullName) || !isValidRender(signature)) defaultValue
        else (fullName, signature)
      case None =>
        val relevantSubexpression = subexpressionForResolvedCallInfo(expr)
        val numArgs               = expr.getValueArguments.size
        val ambiguousReferences =
          Option(bindingContext.get(BindingContext.AMBIGUOUS_REFERENCE_TARGET, relevantSubexpression))
            .map(_.toArray.toSeq.collect { case desc: FunctionDescriptor => desc })
            .getOrElse(Seq())
        val chosenAmbiguousReference = ambiguousReferences.find(_.getValueParameters.size == numArgs)
        chosenAmbiguousReference
          .map { desc =>
            val signature = Defines.UnresolvedSignature
            val fullName  = s"${TypeRenderer.renderFqNameForDesc(desc)}:$signature($numArgs)"
            (fullName, signature)
          }
          .getOrElse(defaultValue)
    }
  }

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String = {
    resolvedCallDescriptor(expr)
      .map(_.getOriginal)
      .map { desc => TypeRenderer.render(desc.getReturnType) }
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtAnnotationEntry, defaultValue: String): String = {
    Option(bindingsForEntity(bindingContext, expr))
      .flatMap(_ => Option(bindingContext.get(BindingContext.ANNOTATION, expr)))
      .map { desc => TypeRenderer.render(desc.getType) }
      .getOrElse(defaultValue)
  }

  def fullNameWithSignature(expr: KtBinaryExpression, defaultValue: (String, String)): (String, String) = {
    resolvedCallDescriptor(expr)
      .map { fnDescriptor =>
        val originalDesc = fnDescriptor.getOriginal
        val renderedParameterTypes =
          originalDesc.getValueParameters.asScala.toSeq
            .map(_.getType)
            .map { t => TypeRenderer.render(t) }
            .mkString(",")
        val renderedReturnType = TypeRenderer.render(originalDesc.getReturnType)
        val signature          = s"$renderedReturnType($renderedParameterTypes)"
        val fullName =
          if (originalDesc.isInstanceOf[ClassConstructorDescriptorImpl]) {
            s"$renderedReturnType.${TypeConstants.initPrefix}:$signature"
          } else {
            val renderedFqName = TypeRenderer.renderFqNameForDesc(originalDesc)
            s"$renderedFqName:$signature"
          }
        if (!isValidRender(fullName) || !isValidRender(signature)) defaultValue
        else (fullName, signature)
      }
      .getOrElse(defaultValue)
  }

  def containingDeclFullName(expr: KtCallExpression): Option[String] = {
    resolvedCallDescriptor(expr)
      .map(_.getContainingDeclaration)
      .map(TypeRenderer.renderFqNameForDesc)
  }

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String = {
    resolvedCallDescriptor(expr)
      .map(_.getContainingDeclaration)
      .map(TypeRenderer.renderFqNameForDesc)
      .getOrElse(defaultValue)
  }

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean = {
    resolvedCallDescriptor(expr).forall(_.getDispatchReceiverParameter == null)
  }

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind = {
    val isStaticBasedOnStructure = expr.getReceiverExpression.isInstanceOf[KtSuperExpression]
    if (isStaticBasedOnStructure) return CallKinds.StaticCall

    val isDynamicBasedOnStructure = expr.getReceiverExpression match {
      case _: KtArrayAccessExpression => true
      case _: KtThisExpression        => true
      case _                          => false
    }
    if (isDynamicBasedOnStructure) return CallKinds.DynamicCall

    resolvedCallDescriptor(expr)
      .map { desc =>
        val isExtension = DescriptorUtils.isExtension(desc)
        val isStatic    = DescriptorUtils.isStaticDeclaration(desc) || hasStaticDesc(expr)

        if (isExtension) CallKinds.ExtensionCall
        else if (isStatic) CallKinds.StaticCall
        else CallKinds.DynamicCall
      }
      .getOrElse(CallKinds.Unknown)
  }

  def isExtensionFn(fn: KtNamedFunction): Boolean = {
    Option(bindingContext.get(BindingContext.FUNCTION, fn))
      .map(DescriptorUtils.isExtension(_))
      .getOrElse(false)
  }

  private def renderTypeForParameterDesc(p: ValueParameterDescriptor): String = {
    val typeUpperBounds =
      Option(TypeUtils.getTypeParameterDescriptorOrNull(p.getType))
        .map(_.getUpperBounds)
        .map(_.asScala)
        .map(_.toList)
        .getOrElse(List())
    if (typeUpperBounds.nonEmpty)
      TypeRenderer.render(typeUpperBounds(0))
    else
      TypeRenderer.render(p.getOriginal.getType)
  }

  def fullNameWithSignature(expr: KtQualifiedExpression, defaultValue: (String, String)): (String, String) = {
    resolvedCallDescriptor(expr) match {
      case Some(fnDescriptor) =>
        val originalDesc = fnDescriptor.getOriginal

        val renderedFqNameForDesc = TypeRenderer.renderFqNameForDesc(fnDescriptor)
        val renderedFqNameMaybe = for {
          extensionReceiverParam <- Option(originalDesc.getExtensionReceiverParameter)
          erpType = extensionReceiverParam.getType
        } yield {
          val typeUpperBounds =
            Option(TypeUtils.getTypeParameterDescriptorOrNull(erpType))
              .map(_.getUpperBounds)
              .map(_.asScala)
              .map(_.toList)
              .getOrElse(List())
          if (erpType.isInstanceOf[ErrorType]) {
            s"${Defines.UnresolvedNamespace}.${expr.getName}"
          } else {
            val rendered =
              if (renderedFqNameForDesc.startsWith(TypeConstants.kotlinApplyPrefix)) TypeConstants.javaLangObject
              else if (typeUpperBounds.size == 1) {
                TypeRenderer.render(typeUpperBounds(0), shouldMapPrimitiveArrayTypes = false, unwrapPrimitives = false)
              } else TypeRenderer.render(erpType, shouldMapPrimitiveArrayTypes = false, unwrapPrimitives = false)
            s"$rendered.${originalDesc.getName}"
          }
        }

        val renderedFqName =
          Option(originalDesc.getDispatchReceiverParameter)
            .map(_.getOriginal)
            .map(_.getContainingDeclaration)
            .map { objDesc =>
              if (DescriptorUtils.isAnonymousObject(objDesc)) {
                s"${TypeRenderer.renderFqNameForDesc(objDesc)}.${originalDesc.getName}"
              } else renderedFqNameMaybe.getOrElse(renderedFqNameForDesc)
            }
            .getOrElse(renderedFqNameMaybe.getOrElse(renderedFqNameForDesc))

        val renderedParameterTypes =
          originalDesc.getValueParameters.asScala.toSeq
            .map(renderTypeForParameterDesc(_))
            .mkString(",")
        val renderedReturnType =
          if (isConstructorDescriptor(originalDesc)) TypeConstants.void
          else if (renderedFqNameForDesc.startsWith(TypeConstants.kotlinApplyPrefix)) TypeConstants.javaLangObject
          else TypeRenderer.render(originalDesc.getReturnType)

        val singleLambdaArgExprMaybe = expr.getSelectorExpression match {
          case c: KtCallExpression if c.getLambdaArguments.size() == 1 =>
            Some(c.getLambdaArguments.get(0).getLambdaExpression)
          case _ => None
        }
        val fullNameSignature = s"$renderedReturnType($renderedParameterTypes)"
        val signature =
          singleLambdaArgExprMaybe
            .map(lambdaInvocationSignature(_, renderedReturnType))
            .getOrElse(fullNameSignature)
        (s"$renderedFqName:$fullNameSignature", signature)
      case None =>
        resolvedCallDescriptor(expr.getReceiverExpression) match {
          case Some(desc) =>
            desc match {
              case _: ClassConstructorDescriptorImpl | _: TypeAliasConstructorDescriptorImpl =>
                expr.getSelectorExpression match {
                  case _: KtNameReferenceExpression => (Operators.fieldAccess, "")
                  case _                            => defaultValue
                }
              case _ =>
                val originalDesc = desc.getOriginal
                val lhsName      = TypeRenderer.render(originalDesc.getReturnType)
                val name         = expr.getSelectorExpression.getFirstChild.getText
                val numArgs = expr.getSelectorExpression match {
                  case c: KtCallExpression => c.getValueArguments.size()
                  case _                   => 0
                }
                val signature = s"${Defines.UnresolvedSignature}($numArgs)"
                val fullName  = s"$lhsName.$name:$signature"
                (fullName, signature)
            }
          case None => defaultValue
        }
    }
  }

  def lambdaInvocationSignature(expr: KtLambdaExpression, returnType: String): String = {
    val hasImplicitParameter = implicitParameterName(expr)
    val params               = expr.getValueParameters
    val paramsString =
      if (hasImplicitParameter.nonEmpty) TypeConstants.javaLangObject
      else if (params.isEmpty) ""
      else if (params.size() == 1) TypeConstants.javaLangObject
      else
        s"${TypeConstants.javaLangObject}${("," + TypeConstants.javaLangObject) * (expr.getValueParameters.size() - 1)}"
    s"${returnType}($paramsString)"
  }

  def parameterType(parameter: KtParameter, defaultValue: String): String = {
    // TODO: add specific test for no binding info of parameter
    // triggered by exception in https://github.com/agrosner/DBFlow
    // TODO: ...also test cases for non-null binding info for other fns
    val render = for {
      mapForEntity <- Option(bindingsForEntity(bindingContext, parameter))
      variableDesc <- Option(mapForEntity.get(BindingContext.VALUE_PARAMETER.getKey))
      typeUpperBounds =
        Option(TypeUtils.getTypeParameterDescriptorOrNull(variableDesc.getType))
          .map(_.getUpperBounds)
          .map(_.asScala)
          .map(_.toList)
          .getOrElse(List())
      render =
        if (typeUpperBounds.nonEmpty)
          TypeRenderer.render(typeUpperBounds(0))
        else
          TypeRenderer.render(variableDesc.getType)
      if isValidRender(render) && !variableDesc.getType.isInstanceOf[ErrorType]
    } yield render

    render.getOrElse(
      Option(parameter.getTypeReference)
        .map { typeRef =>
          typeFromImports(typeRef.getText, parameter.getContainingKtFile).getOrElse(typeRef.getText)
        }
        .getOrElse(defaultValue)
    )
  }

  def destructuringEntryType(expr: KtDestructuringDeclarationEntry, defaultValue: String): String = {
    val render = for {
      mapForEntity <- Option(bindingsForEntity(bindingContext, expr))
      variableDesc <- Option(mapForEntity.get(BindingContext.VARIABLE.getKey))
      render = TypeRenderer.render(variableDesc.getType)
      if isValidRender(render) && !variableDesc.getType.isInstanceOf[ErrorType]
    } yield render
    render.getOrElse(defaultValue)
  }

  def hasApplyOrAlsoScopeFunctionParent(expr: KtLambdaExpression): Boolean = {
    expr.getParent.getParent match {
      case callExpr: KtCallExpression =>
        resolvedCallDescriptor(callExpr) match {
          case Some(desc) =>
            val rendered = TypeRenderer.renderFqNameForDesc(desc.getOriginal)
            rendered.startsWith(TypeConstants.kotlinApplyPrefix) || rendered.startsWith(TypeConstants.kotlinAlsoPrefix)
          case _ => false
        }
      case _ => false
    }
  }

  def returnTypeFullName(expr: KtLambdaExpression): String = {
    TypeConstants.javaLangObject
  }

  def fullNameWithSignature(expr: KtLambdaExpression, lambdaName: String): (String, String) = {
    val containingFile      = expr.getContainingKtFile
    val fileName            = containingFile.getName
    val packageName         = containingFile.getPackageFqName.toString
    val astDerivedFullName  = s"$packageName:<f_$fileName>.$lambdaName()"
    val astDerivedSignature = anySignature(expr.getValueParameters.asScala.toList)

    val render = for {
      mapForEntity <- Option(bindingsForEntity(bindingContext, expr))
      typeInfo     <- Option(mapForEntity.get(BindingContext.EXPRESSION_TYPE_INFO.getKey))
      theType = typeInfo.getType
    } yield {
      val constructorDesc = theType.getConstructor.getDeclarationDescriptor
      val constructorType = constructorDesc.getDefaultType
      val args            = constructorType.getArguments.asScala.drop(1)

      val renderedRetType =
        args.lastOption
          .map { t => TypeRenderer.render(t.getType) }
          .getOrElse(TypeConstants.javaLangObject)
      val renderedArgs =
        if (args.isEmpty) ""
        else if (args.size == 1) TypeConstants.javaLangObject
        else s"${TypeConstants.javaLangObject}${("," + TypeConstants.javaLangObject) * (args.size - 1)}"
      val signature = s"$renderedRetType($renderedArgs)"
      val fullName  = s"$packageName.<f_$fileName>.$lambdaName:$signature"
      (fullName, signature)
    }
    render.getOrElse((astDerivedFullName, astDerivedSignature))
  }

  private def renderedReturnType(fnDesc: FunctionDescriptor): String = {
    val returnT    = fnDesc.getReturnType.getConstructor.getDeclarationDescriptor.getDefaultType
    val typeParams = fnDesc.getTypeParameters.asScala.toList

    val typesInTypeParams = typeParams.map(_.getDefaultType.getConstructor.getDeclarationDescriptor.getDefaultType)
    val hasReturnTypeFromTypeParams = typesInTypeParams.contains(returnT)
    if (hasReturnTypeFromTypeParams) {
      if (returnT.getConstructor.getSupertypes.asScala.nonEmpty) {
        val firstSuperType = returnT.getConstructor.getSupertypes.asScala.toList.head
        TypeRenderer.render(firstSuperType)
      } else {
        val renderedReturnT = TypeRenderer.render(returnT)
        if (renderedReturnT == TypeConstants.tType) TypeConstants.javaLangObject
        else renderedReturnT
      }
    } else {
      TypeRenderer.render(fnDesc.getReturnType)
    }
  }

  def fullNameWithSignature(expr: KtSecondaryConstructor, defaultValue: (String, String)): (String, String) = {
    val fnDesc = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
    val paramTypeNames = expr.getValueParameters.asScala.map { parameter =>
      val explicitTypeFullName =
        Option(parameter.getTypeReference)
          .map(_.getText)
          .map(TypeRenderer.stripped)
          .getOrElse(Defines.UnresolvedNamespace)
      // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
      parameterType(parameter, explicitTypeFullName)
    }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"
    val methodName = fnDesc
      .map(desc => s"${TypeRenderer.renderFqNameForDesc(desc)}${TypeConstants.initPrefix}")
      .getOrElse(s"${Defines.UnresolvedNamespace}.${TypeConstants.initPrefix}")
    val signature = s"${TypeConstants.void}$paramListSignature"
    val fullname  = s"$methodName:$signature"
    (fullname, signature)
  }

  def fullNameWithSignature(expr: KtPrimaryConstructor, defaultValue: (String, String)): (String, String) = {
    // if not explicitly defined, the primary ctor will be `null`
    if (expr == null) {
      return defaultValue
    }
    val paramTypeNames = expr.getValueParameters.asScala.map { parameter =>
      val explicitTypeFullName = Option(parameter.getTypeReference)
        .map(_.getText)
        .getOrElse(Defines.UnresolvedNamespace)
      // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
      parameterType(parameter, TypeRenderer.stripped(explicitTypeFullName))
    }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"

    val methodName = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
      .map { info => s"${TypeRenderer.renderFqNameForDesc(info)}${TypeConstants.initPrefix}" }
      .getOrElse(s"${Defines.UnresolvedNamespace}.${TypeConstants.initPrefix}")
    val signature = s"${TypeConstants.void}$paramListSignature"
    val fullname  = s"$methodName:$signature"
    (fullname, signature)
  }

  def fullNameWithSignatureAsLambda(expr: KtNamedFunction, lambdaName: String): (String, String) = {
    val containingFile      = expr.getContainingKtFile
    val fileName            = containingFile.getName
    val packageName         = containingFile.getPackageFqName.toString
    val astDerivedFullName  = s"$packageName:<f_$fileName>.$lambdaName()"
    val astDerivedSignature = anySignature(expr.getValueParameters.asScala.toList)

    val render = for {
      mapForEntity <- Option(bindingsForEntity(bindingContext, expr))
      typeInfo     <- Option(mapForEntity.get(BindingContext.EXPRESSION_TYPE_INFO.getKey))
      theType = typeInfo.getType
    } yield {
      val constructorDesc = theType.getConstructor.getDeclarationDescriptor
      val constructorType = constructorDesc.getDefaultType
      val args            = constructorType.getArguments.asScala.drop(1)

      val renderedRetType =
        args.lastOption
          .map { t => TypeRenderer.render(t.getType) }
          .getOrElse(TypeConstants.javaLangObject)
      val renderedArgs =
        if (args.isEmpty) ""
        else if (args.size == 1) TypeConstants.javaLangObject
        else s"${TypeConstants.javaLangObject}${("," + TypeConstants.javaLangObject) * (args.size - 1)}"
      val signature = s"$renderedRetType($renderedArgs)"
      val fullName  = s"$packageName.<f_$fileName>.$lambdaName:$signature"
      (fullName, signature)
    }
    render.getOrElse((astDerivedFullName, astDerivedSignature))
  }

  def fullNameWithSignature(expr: KtNamedFunction, defaultValue: (String, String)): (String, String) = {
    val fnDescMaybe        = Option(bindingContext.get(BindingContext.FUNCTION, expr))
    val returnTypeFullName = fnDescMaybe.map(renderedReturnType(_)).getOrElse(Defines.UnresolvedNamespace)
    val paramTypeNames = expr.getValueParameters.asScala.map { parameter =>
      val explicitTypeFullName =
        Option(parameter.getTypeReference)
          .map(_.getText)
          .getOrElse(Defines.UnresolvedNamespace)
      // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
      parameterType(parameter, TypeRenderer.stripped(explicitTypeFullName))
    }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"

    val methodName = for {
      fnDesc                 <- fnDescMaybe
      extensionReceiverParam <- Option(fnDesc.getExtensionReceiverParameter)
      erpType = extensionReceiverParam.getType
    } yield {
      if (erpType.isInstanceOf[ErrorType]) {
        s"${Defines.UnresolvedNamespace}.${expr.getName}"
      } else {
        val theType      = fnDescMaybe.get.getExtensionReceiverParameter.getType
        val renderedType = TypeRenderer.render(theType)
        s"$renderedType.${expr.getName}"
      }
    }

    val nameNoParent = s"${methodName.getOrElse(expr.getFqName)}"
    val name = if (expr.getContext.isInstanceOf[KtClassBody] || expr.isLocal) {
      fnDescMaybe
        .map(_.getContainingDeclaration)
        .map { containingDecl =>
          s"${TypeRenderer.renderFqNameForDesc(containingDecl.getOriginal)}.${expr.getName}"
        }
        .getOrElse(nameNoParent)
    } else nameNoParent
    val signature = s"$returnTypeFullName$paramListSignature"
    val fullname  = s"$name:$signature"
    (fullname, signature)
  }

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean = {
    descriptorForNameReference(expr).exists {
      case _: LazyJavaClassDescriptor => true
      case _: LazyClassDescriptor     => true
      case _                          => false
    }
  }

  private def descriptorForNameReference(expr: KtNameReferenceExpression): Option[DeclarationDescriptor] = {
    Option(bindingsForEntity(bindingContext, expr))
      .map(_ => bindingContext.get(BindingContext.REFERENCE_TARGET, expr))
  }

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .collect { case desc: PropertyDescriptorImpl => TypeRenderer.renderFqNameForDesc(desc.getContainingDeclaration) }
      .getOrElse(defaultValue)
  }

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind = {
    descriptorForNameReference(expr)
      .collect {
        case _: ValueDescriptor                   => NameReferenceKinds.Property
        case _: LazyClassDescriptor               => NameReferenceKinds.ClassName
        case _: LazyJavaClassDescriptor           => NameReferenceKinds.ClassName
        case _: DeserializedClassDescriptor       => NameReferenceKinds.ClassName
        case _: EnumEntrySyntheticClassDescriptor => NameReferenceKinds.EnumEntry
        case unhandled: Any =>
          logger.debug(
            s"Unhandled class in type info fetch in `nameReferenceKind[NameReference]` for `${expr.getText}` with class `${unhandled.getClass}`."
          )
          NameReferenceKinds.Unknown
      }
      .getOrElse(NameReferenceKinds.Unknown)
  }

  def typeFullName(expr: KtPrimaryConstructor, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
      .map { desc => TypeRenderer.render(desc.getReturnType) }
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtSecondaryConstructor, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
      .map { desc => TypeRenderer.render(desc.getReturnType) }
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .flatMap {
        case typedDesc: ValueDescriptor => Some(TypeRenderer.render(typedDesc.getType))
        // TODO: add test cases for the LazyClassDescriptors (`okio` codebase serves as good example)
        case typedDesc: LazyClassDescriptor               => Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: LazyJavaClassDescriptor           => Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: DeserializedClassDescriptor       => Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: EnumEntrySyntheticClassDescriptor => Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: LazyPackageViewDescriptorImpl     => Some(TypeRenderer.renderFqNameForDesc(typedDesc))
        case unhandled: Any =>
          logger.debug(s"Unhandled class type info fetch in for `${expr.getText}` with class `${unhandled.getClass}`.")
          None
        case null => None
      }
      .getOrElse(defaultValue)
  }
  def typeFromImports(name: String, file: KtFile): Option[String] = {
    file.getImportList.getImports.asScala.flatMap { directive =>
      if (directive.getImportedName != null && directive.getImportedName.toString == name.stripSuffix("?"))
        Some(directive.getImportPath.getPathStr)
      else None
    }.headOption
  }

  def implicitParameterName(expr: KtLambdaExpression): Option[String] = {
    if (!expr.getValueParameters.isEmpty) {
      return None
    }

    val hasSingleImplicitParameter =
      Option(bindingContext.get(BindingContext.EXPECTED_EXPRESSION_TYPE, expr))
        .map { desc =>
          // 1 for the parameter + 1 for the return type == 2
          desc.getConstructor.getParameters.size() == 2
        }
        .getOrElse(false)

    val containingQualifiedExpression = Option(expr.getParent)
      .map(_.getParent)
      .flatMap(_.getParent match {
        case q: KtQualifiedExpression => Some(q)
        case _                        => None
      })
    containingQualifiedExpression match {
      case Some(qualifiedExpression) =>
        resolvedCallDescriptor(qualifiedExpression) match {
          case Some(fnDescriptor) =>
            val originalDesc   = fnDescriptor.getOriginal
            val vps            = originalDesc.getValueParameters
            val renderedFqName = TypeRenderer.renderFqNameForDesc(originalDesc)
            if (
              hasSingleImplicitParameter &&
              (renderedFqName.startsWith(TypeConstants.kotlinRunPrefix) ||
                renderedFqName.startsWith(TypeConstants.kotlinApplyPrefix))
            ) {
              Some(TypeConstants.scopeFunctionThisParameterName)
              // https://kotlinlang.org/docs/lambdas.html#it-implicit-name-of-a-single-parameter
            } else if (hasSingleImplicitParameter) {
              Some(TypeConstants.lambdaImplicitParameterName)
            } else None
          case None => None
        }
      case None => None
    }
  }
}

object DefaultTypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  def bindingsForEntity(bindings: BindingContext, entity: KtElement): KeyFMap = {
    try {
      val thisField = bindings.getClass.getDeclaredField("this$0")
      thisField.setAccessible(true)
      val bindingTrace = thisField.get(bindings).asInstanceOf[NoScopeRecordCliBindingTrace]

      val mapField = bindingTrace.getClass.getSuperclass.getSuperclass.getDeclaredField("map")
      mapField.setAccessible(true)
      val map = mapField.get(bindingTrace)

      val mapMapField = map.getClass.getDeclaredField("map")
      mapMapField.setAccessible(true)
      val mapMap = mapMapField.get(map).asInstanceOf[java.util.Map[Object, KeyFMap]]

      val mapForEntity = mapMap.get(entity)
      mapForEntity
    } catch {
      case noSuchField: NoSuchFieldException =>
        logger.debug(
          s"Encountered _no such field_ exception while retrieving type info for `${entity.getName}`: `$noSuchField`."
        )
        KeyFMap.EMPTY_MAP
      case e if NonFatal(e) =>
        logger.debug(s"Encountered general exception while retrieving type info for `${entity.getName}`: `$e`.")
        KeyFMap.EMPTY_MAP
    }
  }

  def bindingsForEntityAsString(bindings: BindingContext, entity: KtElement): String = {
    val mapForEntity = bindingsForEntity(bindings, entity)
    if (mapForEntity != null) {
      val keys = mapForEntity.getKeys
      entity.toString + ": " + entity.getText + "\n" +
        keys.map(key => s"$key: ${mapForEntity.get(key)}").mkString("  ", "\n  ", "")
    } else {
      "No entries"
    }
  }

  def printBindingsForEntity(bindings: BindingContext, entity: KtElement): Unit = {
    println(bindingsForEntityAsString(bindings, entity))
  }
}
