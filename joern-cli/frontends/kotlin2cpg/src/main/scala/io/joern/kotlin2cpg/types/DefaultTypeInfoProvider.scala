package io.joern.kotlin2cpg.types

import io.shiftleft.passes.KeyPool
import org.jetbrains.kotlin.cli.jvm.compiler.{
  KotlinCoreEnvironment,
  KotlinToJVMBytecodeCompiler,
  NoScopeRecordCliBindingTrace
}
import org.jetbrains.kotlin.com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor, FunctionDescriptor, ValueDescriptor}
import org.jetbrains.kotlin.descriptors.impl.{
  ClassConstructorDescriptorImpl,
  EnumEntrySyntheticClassDescriptor,
  PropertyDescriptorImpl,
  TypeAliasConstructorDescriptorImpl
}
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaClassDescriptor
import org.jetbrains.kotlin.load.java.sources.JavaSourceElement
import org.jetbrains.kotlin.load.java.structure.impl.classFiles.BinaryJavaMethod
import org.jetbrains.kotlin.psi.{
  KtArrayAccessExpression,
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtExpression,
  KtLambdaExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtParameter,
  KtPrimaryConstructor,
  KtProperty,
  KtQualifiedExpression,
  KtSecondaryConstructor,
  KtSuperExpression,
  KtThisExpression,
  KtTypeAlias,
  KtTypeReference
}
import org.jetbrains.kotlin.resolve.{BindingContext, DescriptorUtils}
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.NoDescriptorForDeclarationException
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor
import org.jetbrains.kotlin.types.UnresolvedType
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala

class DefaultTypeInfoProvider(environment: KotlinCoreEnvironment) extends TypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  import DefaultTypeInfoProvider._

  // TODO: remove this state
  var hasEmptyBindingContext = false

  lazy val bindingContext: BindingContext = {
    logger.info("Running Kotlin compiler analysis...")
    try {
      val t0  = System.currentTimeMillis()
      val res = KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      val t1  = System.currentTimeMillis()
      logger.info("Kotlin compiler analysis finished in `" + (t1 - t0) + "` ms.")
      res.getBindingContext
    } catch {
      case noDesc: NoDescriptorForDeclarationException =>
        logger.error("Kotlin compiler analysis failed with _missing declaration_ exception `" + noDesc.toString + "`.")
        hasEmptyBindingContext = true
        BindingContext.EMPTY
      case e: Throwable =>
        logger.error("Kotlin compiler analysis failed with exception `" + e.toString + "`:` + " + e.getMessage + " +`.")
        hasEmptyBindingContext = true
        BindingContext.EMPTY
    }
  }

  def isValidRender(render: String): Boolean = {
    !render.contains("ERROR")
  }

  def erasedSignature(args: Seq[Any]): String = {
    val argsSignature = {
      if (args.isEmpty) {
        ""
      } else if (args.size == 1) {
        TypeConstants.any
      } else {
        TypeConstants.any + ("," + TypeConstants.any) * (args.size - 1)
      }
    }
    TypeConstants.any + "(" + argsSignature + ")"
  }

  def fullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(TypeRenderer.renderFqName)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    val isStatic =
      resolvedDesc
        .map(_.getSource)
        .collect { case s: JavaSourceElement => s }
        .map(_.getJavaElement)
        .collect { case bjm: BinaryJavaMethod => bjm }
        .map(_.isStatic)
        .getOrElse(false)
    isStatic
  }

  def fullNameWithSignature(expr: KtDestructuringDeclarationEntry, defaultValue: (String, String)): (String, String) = {
    val resolvedCall = bindingContext.get(BindingContext.COMPONENT_RESOLVED_CALL, expr)
    if (resolvedCall != null) {
      val fnDesc = resolvedCall.getResultingDescriptor
      val relevantDesc =
        if (!fnDesc.isActual && fnDesc.getOverriddenDescriptors.asScala.nonEmpty) {
          fnDesc.getOverriddenDescriptors.asScala.toList.head
        } else {
          fnDesc
        }
      val renderedFqName     = TypeRenderer.renderFqName(relevantDesc)
      val returnTypeFullName = renderedReturnType(relevantDesc.getOriginal)

      val renderedParameterTypes =
        relevantDesc.getValueParameters.asScala.toSeq
          .map { valueParam =>
            TypeRenderer.render(valueParam.getOriginal.getType)
          }
          .mkString(",")
      val signature = returnTypeFullName + "(" + renderedParameterTypes + ")"
      val fullName  = s"$renderedFqName:$signature"

      if (!isValidRender(fullName) || !isValidRender(signature)) {
        defaultValue
      } else {
        (fullName, signature)
      }
    } else {
      defaultValue
    }
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
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val originalDesc = fnDescriptor.getOriginal

        val isCtor = originalDesc match {
          case _: ClassConstructorDescriptorImpl     => true
          case _: TypeAliasConstructorDescriptorImpl => true
          case _                                     => false
        }
        val relevantDesc =
          if (!originalDesc.isActual && originalDesc.getOverriddenDescriptors.asScala.nonEmpty) {
            originalDesc.getOverriddenDescriptors.asScala.toList.head
          } else {
            originalDesc
          }
        val returnTypeFullName = {
          if (isCtor) {
            TypeConstants.void
          } else {
            renderedReturnType(relevantDesc.getOriginal)
          }
        }
        returnTypeFullName
      case None => defaultValue
    }
  }

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(_.getExpandedType)
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
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def inheritanceTypes(expr: KtClassOrObject, defaultValue: Seq[String]): Seq[String] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(getSuperclassDescriptors)
      .filter(_.asScala.nonEmpty)
      .map(_.asScala.map { superClassDesc =>
        TypeRenderer.render(superClassDesc.getDefaultType)
      }.toList)
      .getOrElse(defaultValue)
  }

  def fullName(expr: KtClassOrObject, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(_.getDefaultType)
      .map(TypeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
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
    val typeInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (typeInfo != null && typeInfo.getType != null && typeInfo.getType.getArguments.asScala.nonEmpty) {
      val firstTypeArg = typeInfo.getType.getArguments.get(0)
      val rendered     = TypeRenderer.render(firstTypeArg.getType)
      val retType      = expressionType(expr, TypeConstants.any)
      val signature    = s"$retType()"
      val fullName     = s"$rendered.${TypeConstants.classLiteralReplacementMethodName}:$signature"
      (fullName, signature)
    } else {
      defaultValue
    }
  }

  private def subexpressionForResolvedCallInfo(expr: KtExpression): KtExpression = {
    expr match {
      case typedExpr: KtCallExpression =>
        Option(typedExpr.getFirstChild)
          .collect { case firstChild: KtExpression =>
            firstChild
          }
          .getOrElse(expr)
      case typedExpr: KtQualifiedExpression =>
        Option(typedExpr.getSelectorExpression)
          .collect { case call: KtCallExpression =>
            subexpressionForResolvedCallInfo(call)
          }
          .getOrElse(typedExpr)
      case typedExpr: KtBinaryExpression =>
        Option(typedExpr.getChildren.toList(1))
          .collect { case secondChild: KtExpression =>
            secondChild
          }
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

    descMaybe.collect { case desc: FunctionDescriptor =>
      desc
    }
  }

  def isConstructorCall(expr: KtCallExpression): Option[Boolean] = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        fnDescriptor match {
          case _: ClassConstructorDescriptorImpl     => Some(true)
          case _: TypeAliasConstructorDescriptorImpl => Some(true)
          case _                                     => Some(false)
        }
      case None => None
    }
  }

  def fullNameWithSignature(expr: KtCallExpression, defaultValue: (String, String)): (String, String) = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val originalDesc = fnDescriptor.getOriginal

        val isCtor = originalDesc match {
          case _: ClassConstructorDescriptorImpl     => true
          case _: TypeAliasConstructorDescriptorImpl => true
          case _                                     => false
        }

        val relevantDesc =
          originalDesc match {
            case typedDesc: TypeAliasConstructorDescriptorImpl =>
              typedDesc.getUnderlyingConstructorDescriptor
            case typedDesc: FunctionDescriptor =>
              if (!typedDesc.isActual && typedDesc.getOverriddenDescriptors.asScala.nonEmpty) {
                typedDesc.getOverriddenDescriptors.asScala.toList.head
              } else {
                typedDesc
              }
          }

        // TODO: write descriptor renderer instead of working with the existing ones
        // that render comments in fqnames
        val renderedFqName = TypeRenderer.renderFqName(relevantDesc)
        val returnTypeFullName = {
          if (isCtor) {
            TypeConstants.void
          } else {
            renderedReturnType(relevantDesc.getOriginal)
          }
        }

        val renderedParameterTypes =
          relevantDesc.getValueParameters.asScala.toSeq
            .map { valueParam =>
              TypeRenderer.render(valueParam.getOriginal.getType)
            }
            .mkString(",")
        val signature = returnTypeFullName + "(" + renderedParameterTypes + ")"
        val fullName =
          if (isCtor) {
            s"$renderedFqName${TypeConstants.initPrefix}:$signature"
          } else {
            s"$renderedFqName:$signature"
          }

        if (!isValidRender(fullName) || !isValidRender(signature)) {
          defaultValue
        } else {
          (fullName, signature)
        }
      case None =>
        defaultValue
    }
  }

  def typeFullName(expr: KtBinaryExpression, defaultValue: String): String = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val originalDesc = fnDescriptor.getOriginal
        TypeRenderer.render(originalDesc.getReturnType)
      case None => defaultValue
    }
  }

  def fullNameWithSignature(expr: KtBinaryExpression, defaultValue: (String, String)): (String, String) = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val originalDesc = fnDescriptor.getOriginal
        // TODO: write descriptor renderer instead of working with the existing ones
        // that render comments in fqnames
        val renderedParameterTypes =
          originalDesc.getValueParameters.asScala.toSeq
            .map { valueParam =>
              val t = valueParam.getType
              TypeRenderer.render(t)
            }
            .mkString(",")
        val renderedReturnType = TypeRenderer.render(originalDesc.getReturnType)
        val signature          = renderedReturnType + "(" + renderedParameterTypes + ")"
        val fullName =
          if (originalDesc.isInstanceOf[ClassConstructorDescriptorImpl]) {
            s"$renderedReturnType.${TypeConstants.initPrefix}:$signature"
          } else {
            val renderedFqName = TypeRenderer.renderFqName(originalDesc)
            s"$renderedFqName:$signature"
          }
        if (!isValidRender(fullName) || !isValidRender(signature)) {
          defaultValue
        } else {
          (fullName, signature)
        }
      case None => defaultValue
    }
  }

  def containingDeclFullName(expr: KtCallExpression): Option[String] = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc.map { fnDescriptor =>
      val decl = fnDescriptor.getContainingDeclaration
      TypeRenderer.renderFqName(decl)
    }
  }

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val decl = fnDescriptor.getContainingDeclaration
        TypeRenderer.renderFqName(decl)
      case None => defaultValue
    }
  }

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        fnDescriptor.getDispatchReceiverParameter == null
      case _ => true
    }
  }

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind = {
    val isStaticBasedOnStructure = {
      expr.getReceiverExpression match {
        case _: KtSuperExpression => true
        case _                    => false
      }
    }
    if (isStaticBasedOnStructure) return CallKinds.StaticCall

    val isDynamicBasedOnStructure =
      expr.getReceiverExpression match {
        case _: KtArrayAccessExpression => true
        case _: KtThisExpression        => true
        case _                          => false
      }
    if (isDynamicBasedOnStructure) return CallKinds.DynamicCall

    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val isExtension = DescriptorUtils.isExtension(fnDescriptor)
        val isStatic    = DescriptorUtils.isStaticDeclaration(fnDescriptor) || hasStaticDesc(expr)

        if (isExtension) CallKinds.ExtensionCall
        else if (isStatic) CallKinds.StaticCall
        else CallKinds.DynamicCall
      case None => CallKinds.Unknown
    }
  }

  def fullNameWithSignature(expr: KtQualifiedExpression, defaultValue: (String, String)): (String, String) = {
    resolvedCallDescriptor(expr) match {
      case Some(fnDescriptor) =>
        val originalDesc          = fnDescriptor.getOriginal
        val renderedFqNameForDesc = TypeRenderer.renderFqName(originalDesc)
        val renderedFqName =
          if (originalDesc.getExtensionReceiverParameter != null) {
            val extType = originalDesc.getExtensionReceiverParameter.getType
            val extName = originalDesc.getName
            val rendered =
              if (renderedFqNameForDesc.startsWith(TypeConstants.kotlinApplyPrefix)) {
                TypeConstants.javaLangObject
              } else {
                TypeRenderer.render(extType, false)
              }
            s"$rendered.$extName"
          } else {
            renderedFqNameForDesc
          }
        val valueParameters = originalDesc.getValueParameters.asScala.toSeq
        val renderedParameterTypes =
          valueParameters
            .map { valueParam =>
              TypeRenderer.render(valueParam.getType)
            }
            .mkString(",")
        val renderedReturnType =
          if (renderedFqNameForDesc.startsWith(TypeConstants.kotlinApplyPrefix)) {
            // TODO: handle `T` in Kotlin stdlib's `apply`
            TypeConstants.javaLangObject
          } else {
            TypeRenderer.render(originalDesc.getReturnType)
          }
        val signature = s"$renderedReturnType($renderedParameterTypes)"
        val fn        = (s"$renderedFqName:$signature", signature)
        fn
      case None => defaultValue
    }
  }

  def parameterType(expr: KtParameter, defaultValue: String): String = {
    // TODO: add specific test for no binding info of parameter
    // triggered by exception in https://github.com/agrosner/DBFlow
    // TODO: ...also test cases for non-null binding info for other fns
    val render = for {
      mapForEntity <- Option(bindingsForEntity(bindingContext, expr))
      variableDesc <- Option(mapForEntity.get(BindingContext.VALUE_PARAMETER.getKey))
      render = TypeRenderer.render(variableDesc.getType)
      if isValidRender(render)
    } yield render

    render.getOrElse(defaultValue)
  }

  def returnTypeFullName(expr: KtLambdaExpression): String = {
    TypeConstants.javaLangObject
  }

  def fullNameWithSignature(expr: KtLambdaExpression, keyPool: KeyPool): (String, String) = {
    val containingFile = expr.getContainingKtFile
    val fileName       = containingFile.getName
    val lambdaNum      = keyPool.next
    val astDerivedFullName =
      containingFile.getPackageFqName.toString + ":" + "<lambda>" + "<f_" + fileName + "_no" + lambdaNum + ">" + "()"
    val astDerivedSignature = erasedSignature(expr.getValueParameters.asScala.toList)

    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity == null || mapForEntity.getKeys == null) {
      return (astDerivedFullName, astDerivedSignature)
    }
    val typeInfo        = mapForEntity.get(BindingContext.EXPRESSION_TYPE_INFO.getKey)
    val theType         = typeInfo.getType
    val constructorDesc = theType.getConstructor.getDeclarationDescriptor
    val constructorType = constructorDesc.getDefaultType
    val args            = constructorType.getArguments.asScala.drop(1)
    val renderedArgs =
      if (args.isEmpty) {
        ""
      } else if (args.size == 1) {
        TypeConstants.javaLangObject
      } else {
        s"${TypeConstants.javaLangObject}${("," + TypeConstants.javaLangObject) * (args.size - 1)}"
      }
    val signature = TypeConstants.javaLangObject + "(" + renderedArgs + ")"
    val fullName =
      containingFile.getPackageFqName.toString + ".<lambda><f_" + fileName + "_no" + lambdaNum.toString + ">" + ":" + signature
    (fullName, signature)
  }

  private def renderedReturnType(fnDesc: FunctionDescriptor): String = {
    val typeParams        = fnDesc.getTypeParameters.asScala.toList
    val returnT           = fnDesc.getReturnType.getConstructor.getDeclarationDescriptor.getDefaultType
    val typesInTypeParams = typeParams.map(_.getDefaultType.getConstructor.getDeclarationDescriptor.getDefaultType)
    val hasReturnTypeFromTypeParams = typesInTypeParams.contains(returnT)
    if (hasReturnTypeFromTypeParams) {
      if (returnT.getConstructor.getSupertypes.asScala.nonEmpty) {
        val firstSuperType = returnT.getConstructor.getSupertypes.asScala.toList.head
        TypeRenderer.render(firstSuperType)
      } else {
        val renderedReturnT = TypeRenderer.render(returnT)
        if (renderedReturnT == TypeConstants.tType) {
          TypeConstants.javaLangObject
        } else {
          renderedReturnT
        }
      }
    } else {
      TypeRenderer.render(fnDesc.getReturnType)
    }
  }

  def fullNameWithSignature(expr: KtSecondaryConstructor, defaultValue: (String, String)): (String, String) = {
    val fnDesc = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
    val paramTypeNames =
      try {
        val nodeParams = expr.getValueParameters
        nodeParams.asScala
          .map { p =>
            val explicitTypeFullName = if (p.getTypeReference != null) {
              p.getTypeReference.getText
            } else {
              TypeConstants.cpgUnresolved
            }
            // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
            val typeFullName = parameterType(p, TypeRenderer.stripped(explicitTypeFullName))
            typeFullName
          }
      } catch {
        case _: Throwable => List()
      }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"

    val methodName =
      if (fnDesc.isEmpty) {
        TypeConstants.cpgUnresolved + "." + TypeConstants.initPrefix
      } else {
        TypeRenderer.renderFqName(fnDesc.get) + TypeConstants.initPrefix
      }
    val signature = TypeConstants.void + paramListSignature
    val fullname  = s"$methodName:$signature"
    (fullname, signature)
  }

  def fullNameWithSignature(expr: KtPrimaryConstructor, defaultValue: (String, String)): (String, String) = {
    // if not explicitly defined, the primary ctor will be `null`
    if (expr == null) {
      return defaultValue
    }
    val fnDesc = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
    val paramTypeNames =
      try {
        val nodeParams = expr.getValueParameters
        nodeParams.asScala
          .map { p =>
            val explicitTypeFullName = if (p.getTypeReference != null) {
              p.getTypeReference.getText
            } else {
              TypeConstants.cpgUnresolved
            }
            // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
            val typeFullName = parameterType(p, TypeRenderer.stripped(explicitTypeFullName))
            typeFullName
          }
      } catch {
        case _: Throwable => List()
      }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"

    val methodName =
      if (fnDesc.isEmpty) {
        TypeConstants.cpgUnresolved + "." + TypeConstants.initPrefix
      } else {
        TypeRenderer.renderFqName(fnDesc.get) + TypeConstants.initPrefix
      }
    val signature = TypeConstants.void + paramListSignature
    val fullname  = s"$methodName:$signature"
    (fullname, signature)
  }

  def fullNameWithSignature(expr: KtNamedFunction, defaultValue: (String, String)): (String, String) = {
    val fnDesc = Option(bindingContext.get(BindingContext.FUNCTION, expr))
    val returnTypeFullName =
      fnDesc match {
        case Some(desc) => renderedReturnType(desc)
        case None       => TypeConstants.cpgUnresolved
      }
    val paramTypeNames =
      try {
        val nodeParams = expr.getValueParameters
        nodeParams.asScala
          .map { p =>
            val explicitTypeFullName = if (p.getTypeReference != null) {
              p.getTypeReference.getText
            } else {
              TypeConstants.cpgUnresolved
            }
            // TODO: return all the parameter types in this fn for registration, otherwise they will be missing
            val typeFullName = parameterType(p, TypeRenderer.stripped(explicitTypeFullName))
            typeFullName
          }
      } catch {
        case _: Throwable => List()
      }
    val paramListSignature = s"(${paramTypeNames.mkString(",")})"

    val methodName =
      if (fnDesc.isDefined && fnDesc.get.getExtensionReceiverParameter != null) {
        val erpType = fnDesc.get.getExtensionReceiverParameter.getType
        if (erpType.isInstanceOf[UnresolvedType]) {
          TypeConstants.cpgUnresolved + "." + expr.getName
        } else {
          val theType      = fnDesc.get.getExtensionReceiverParameter.getType
          val renderedType = TypeRenderer.render(theType)
          renderedType + "." + expr.getName
        }
      } else {
        expr.getFqName
      }

    val signature = returnTypeFullName + paramListSignature
    val fullname  = s"$methodName:$signature"
    (fullname, signature)
  }

  def isReferenceToClass(expr: KtNameReferenceExpression): Boolean = {
    descriptorForNameReference(expr)
      .collect {
        case _: LazyJavaClassDescriptor =>
          true
        case _: LazyClassDescriptor =>
          true
      }
      .getOrElse(false)
  }

  private def descriptorForNameReference(expr: KtNameReferenceExpression): Option[DeclarationDescriptor] = {
    Option(bindingsForEntity(bindingContext, expr))
      .map(_ => bindingContext.get(BindingContext.REFERENCE_TARGET, expr))
  }

  def referenceTargetTypeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .collect { case desc: ValueDescriptor =>
        desc match {
          case typedDesc: PropertyDescriptorImpl =>
            TypeRenderer.renderFqName(typedDesc.getContainingDeclaration)
          case _ => defaultValue
        }
      }
      .getOrElse(defaultValue)
  }

  def isReferencingMember(expr: KtNameReferenceExpression): Boolean = {
    descriptorForNameReference(expr)
      .collect { case desc: ValueDescriptor =>
        desc match {
          case _: PropertyDescriptorImpl => true
          case _                         => false
        }
      }
      .getOrElse(false)
  }

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind = {
    descriptorForNameReference(expr)
      .collect {
        case _: ValueDescriptor =>
          NameReferenceKinds.Property
        case _: LazyClassDescriptor =>
          NameReferenceKinds.ClassName
        case _: LazyJavaClassDescriptor =>
          NameReferenceKinds.ClassName
        case _: DeserializedClassDescriptor =>
          NameReferenceKinds.ClassName
        case _: EnumEntrySyntheticClassDescriptor =>
          NameReferenceKinds.EnumEntry
        case unhandled: Any =>
          logger.debug(
            s"Unhandled class in type info fetch in `nameReferenceKind[NameReference]` for `${expr.getText}` with class `${unhandled.getClass}`."
          )
          NameReferenceKinds.Unknown
      }
      .getOrElse(NameReferenceKinds.Unknown)
  }

  def typeFullName(expr: KtPrimaryConstructor, defaultValue: String): String = {
    val fnDesc = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
    fnDesc match {
      case Some(desc) => TypeRenderer.render(desc.getReturnType)
      case _          => defaultValue
    }
  }

  def typeFullName(expr: KtSecondaryConstructor, defaultValue: String): String = {
    val fnDesc = Option(bindingContext.get(BindingContext.CONSTRUCTOR, expr))
    fnDesc match {
      case Some(desc) => TypeRenderer.render(desc.getReturnType)
      case _          => defaultValue
    }
  }

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .flatMap {
        case typedDesc: ValueDescriptor =>
          Some(TypeRenderer.render(typedDesc.getType))
        // TODO: add test cases for the LazyClassDescriptors (`okio` codebase serves as good example)
        case typedDesc: LazyClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: LazyJavaClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: DeserializedClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: EnumEntrySyntheticClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType))
        case unhandled: Any =>
          logger.debug(
            s"Unhandled class type info fetch in `typeFullName[NameReference]` for `${expr.getText}` with class `${unhandled.getClass}`."
          )
          None
        case _ => None
      }
      .getOrElse(defaultValue)
  }

  def implicitParameterName(expr: KtLambdaExpression): Option[String] = {
    if (!expr.getValueParameters.isEmpty) {
      return None
    }

    val containingQualifiedExpression =
      Option(expr.getParent)
        .map(_.getParent)
        .map(_.getParent match {
          case q: KtQualifiedExpression => Some(q)
          case _                        => None
        })
        .getOrElse(None)
    containingQualifiedExpression match {
      case Some(qualifiedExpression) =>
        resolvedCallDescriptor(qualifiedExpression) match {
          case Some(fnDescriptor) =>
            val originalDesc   = fnDescriptor.getOriginal
            val renderedFqName = TypeRenderer.renderFqName(originalDesc)
            if (
              renderedFqName.startsWith(TypeConstants.kotlinLetPrefix) ||
              renderedFqName.startsWith(TypeConstants.kotlinAlsoPrefix)
            ) {
              Some(TypeConstants.scopeFunctionItParameterName)
            } else if (
              renderedFqName.startsWith(TypeConstants.kotlinRunPrefix) ||
              renderedFqName.startsWith(TypeConstants.kotlinApplyPrefix)
            ) {
              Some(TypeConstants.scopeFunctionThisParameterName)
            } else {
              None
            }
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
          "Encountered _no such field_ exception while retrieving type info for `" + entity.getName + "`: `" + noSuchField + "`."
        )
        KeyFMap.EMPTY_MAP
      case e: Throwable =>
        logger.debug(
          "Encountered general exception while retrieving type info for `" + entity.getName + "`: `" + e + "`."
        )
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
