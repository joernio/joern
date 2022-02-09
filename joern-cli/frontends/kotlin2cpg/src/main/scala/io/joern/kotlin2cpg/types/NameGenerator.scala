package io.joern.kotlin2cpg.types

import com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.{DeclarationDescriptor, FunctionDescriptor, ValueDescriptor}
import org.jetbrains.kotlin.descriptors.impl.{
  ClassConstructorDescriptorImpl,
  EnumEntrySyntheticClassDescriptor,
  TypeAliasConstructorDescriptorImpl
}
import org.jetbrains.kotlin.psi.{
  KtBinaryExpression,
  KtCallExpression,
  KtClassLiteralExpression,
  KtClassOrObject,
  KtElement,
  KtExpression,
  KtLambdaExpression,
  KtNameReferenceExpression,
  KtNamedFunction,
  KtParameter,
  KtProperty,
  KtQualifiedExpression,
  KtSuperExpression,
  KtTypeAlias,
  KtTypeReference
}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.{LazyClassDescriptor, LazyTypeAliasDescriptor}
import org.jetbrains.kotlin.types.{ErrorType, ErrorUtils, KotlinType, SimpleType, UnresolvedType}
import org.jetbrains.kotlin.cli.jvm.compiler.{
  KotlinCoreEnvironment,
  KotlinToJVMBytecodeCompiler,
  NoScopeRecordCliBindingTrace
}
import org.jetbrains.kotlin.renderer.{DescriptorRenderer, DescriptorRendererImpl, DescriptorRendererOptionsImpl}
import org.jetbrains.kotlin.resolve.BindingContext

import scala.jdk.CollectionConverters._
import org.slf4j.LoggerFactory
import DefaultNameGenerator._
import io.shiftleft.passes.KeyPool
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaClassDescriptor
import org.jetbrains.kotlin.resolve.`lazy`.NoDescriptorForDeclarationException
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor

// representative of `LazyJavaClassDescriptor`, `DeserializedClassDescriptor`, `TypeAliasConstructorDescriptor`, etc.
trait WithDefaultType {
  def getDefaultType(): SimpleType
}

object Constants {
  val kotlinAny = "kotlin.Any"
  val kotlinString = "kotlin.String"
  val cpgUnresolved = "codepropertygraph.Unresolved"
  val any = "ANY"
  val void = "void"
  val classLiteralReplacementMethodName = "getClass"
  val tType = "T"
  val javaLangObject = "java.lang.Object"
  val kotlinFunctionXPrefix = "kotlin.Function"
  val kotlinSuspendFunctionXPrefix = "kotlin.coroutines.SuspendFunction"
  val kotlinApplyPrefix = "kotlin.apply"
  val initPrefix = "<init>"
}

object CallKinds extends Enumeration {
  type CallKind = Value
  val Unknown, StaticCall, DynamicCall, ExtensionCall = Value
}

object NameReferenceKinds extends Enumeration {
  type NameReferenceKind = Value
  val Unknown, ClassName, EnumEntry, LocalVariable, Property = Value
}

trait NameGenerator {
  def returnType(elem: KtNamedFunction, or: String): String

  def containingDeclType(expr: KtQualifiedExpression, or: String): String

  def expressionType(expr: KtExpression, or: String): String

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String]

  def parameterType(expr: KtParameter, or: String): String

  def propertyType(expr: KtProperty, or: String): String

  def fullName(expr: KtClassOrObject, or: String): String

  def fullName(expr: KtTypeAlias, or: String): String

  def aliasTypeFullName(expr: KtTypeAlias, or: String): String

  def typeFullName(expr: KtNameReferenceExpression, or: String): String

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind

  def fullNameWithSignature(expr: KtQualifiedExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtCallExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(call: KtBinaryExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String)

  def fullNameWithSignature(expr: KtLambdaExpression, keyPool: KeyPool): (String, String)

  def erasedSignature(args: Seq[Any]): String

  def returnTypeFullName(expr: KtLambdaExpression): String

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind

  def isConstructorCall(expr: KtCallExpression): Option[Boolean]

  def typeFullName(expr: KtTypeReference, or: String): String
}

object DefaultNameGenerator {
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

object TypeRenderer {
  private val cpgUnresolvedType = ErrorUtils.createUnresolvedType(Constants.cpgUnresolved, List().asJava)

  def descriptorRenderer(): DescriptorRenderer = {
    val opts = new DescriptorRendererOptionsImpl
    opts.setParameterNamesInFunctionalTypes(false)
    opts.setInformativeErrorType(false)
    opts.setTypeNormalizer {
      case _: UnresolvedType => cpgUnresolvedType
      case _: ErrorType      => cpgUnresolvedType
      case t                 => t
    }
    new DescriptorRendererImpl(opts)
  }

  def renderFqName(desc: DeclarationDescriptor): String = {
    val renderer = descriptorRenderer()
    val fqName = DescriptorUtils.getFqName(desc)
    stripped(renderer.renderFqName(fqName))
  }

  def render(t: KotlinType): String = {
    val renderer = descriptorRenderer()
    if (isFunctionXType(t)) {
      Constants.kotlinFunctionXPrefix + (t.getArguments.size() - 1).toString
    } else {
      val rendered = renderer.renderType(t)
      stripped(rendered)
    }
  }

  private def isFunctionXType(t: KotlinType): Boolean = {
    val renderer = descriptorRenderer()
    val renderedConstructor = renderer.renderTypeConstructor(t.getConstructor)
    renderedConstructor.startsWith(Constants.kotlinFunctionXPrefix) ||
    renderedConstructor.startsWith(Constants.kotlinSuspendFunctionXPrefix)
  }

  def stripped(typeName: String): String = {
    def stripTypeParams(typeName: String): String = {
      typeName.replaceAll("<.*>", "")
    }
    def stripOut(name: String): String = {
      if (name.contains("<") && name.contains(">") && name.contains("out")) {
        name.replaceAll("(<[^o]*)[(]?out[)]?[ ]*([a-zA-Z])", "<$2")
      } else {
        name
      }
    }
    def stripOptionality(typeName: String): String = {
      typeName.replaceAll("!", "").replaceAll("\\?", "")
    }
    def stripDebugInfo(typeName: String): String = {
      if (typeName.contains("/* =")) {
        typeName.split("/\\* =")(0)
      } else {
        typeName
      }
    }

    stripTypeParams(stripOptionality(stripDebugInfo(stripOut(typeName))).trim().replaceAll(" ", ""))
  }
}

class DefaultNameGenerator(environment: KotlinCoreEnvironment) extends NameGenerator {
  private val logger = LoggerFactory.getLogger(getClass)

  // TODO: remove this state
  var hasEmptyBindingContext = false

  lazy val bindingContext = {
    logger.info("Running Kotlin compiler analysis...")
    try {
      val t0 = System.currentTimeMillis()
      val res = KotlinToJVMBytecodeCompiler.INSTANCE.analyze(environment)
      val t1 = System.currentTimeMillis()
      logger.info("Kotlin compiler analysis finished in `" + (t1 - t0) + "` ms.")
      res.getBindingContext
    } catch {
      case noDesc: NoDescriptorForDeclarationException =>
        logger.error(
          "Kotlin compiler analysis failed with _missing declaration_ exception `" + noDesc.toString + "`."
        )
        hasEmptyBindingContext = true
        BindingContext.EMPTY
      case e: Throwable =>
        logger.error("Kotlin compiler analysis failed with exception `" + e.toString + "`.")
        hasEmptyBindingContext = true
        BindingContext.EMPTY
    }
  }

  def isValidRender(render: String): Boolean = {
    !render.contains("ERROR")
  }

  def erasedSignature(args: Seq[Any]): String = {
    val argsSignature = {
      if (args.size == 0) {
        ""
      } else if (args.size == 1) {
        Constants.any
      } else {
        Constants.any + ("," + Constants.any) * (args.size - 1)
      }
    }
    Constants.any + "(" + argsSignature + ")"
  }

  def fullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(TypeRenderer.renderFqName)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtTypeReference, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE.getKey))
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def aliasTypeFullName(expr: KtTypeAlias, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE_ALIAS.getKey))
      .map(_.getUnderlyingType)
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def returnType(expr: KtNamedFunction, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.FUNCTION, expr))
      .map(_.getReturnType)
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def propertyType(expr: KtProperty, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.VARIABLE.getKey))
      .map(_.getType)
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def inheritanceTypes(expr: KtClassOrObject, defaultValue: Seq[String]): Seq[String] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(getSuperclassDescriptors)
      .filter(_.size() > 0)
      .map(_.asScala.map { superClassDesc =>
        TypeRenderer.render(superClassDesc.getDefaultType)
      }.toList)
      .getOrElse(defaultValue)
  }

  def fullName(expr: KtClassOrObject, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.CLASS.getKey))
      .map(_.getDefaultType)
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def expressionType(expr: KtExpression, defaultValue: String): String = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .flatMap(tpeInfo => Option(tpeInfo.getType))
      .map(TypeRenderer.render)
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def fullNameWithSignature(expr: KtClassLiteralExpression, defaultValue: (String, String)): (String, String) = {
    val typeInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (typeInfo != null && typeInfo.getType != null && typeInfo.getType.getArguments.size() > 0) {
      val firstTypeArg = typeInfo.getType.getArguments.get(0)
      val rendered = TypeRenderer.render(firstTypeArg.getType)
      val retType = expressionType(expr, Constants.any)
      val signature = s"$retType()"
      val fullName = s"$rendered.${Constants.classLiteralReplacementMethodName}:$signature"
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
      callForSubexpression <- Option(bindingContext.get(BindingContext.CALL, relevantSubexpression))
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
        val isCtor = fnDescriptor match {
          case _: ClassConstructorDescriptorImpl     => true
          case _: TypeAliasConstructorDescriptorImpl => true
          case _                                     => false
        }
        val relevantDesc =
          if (!fnDescriptor.isActual && fnDescriptor.getOverriddenDescriptors.size() > 0) {
            fnDescriptor.getOverriddenDescriptors.asScala.toList.head
          } else {
            fnDescriptor
          }

        // TODO: write descriptor renderer instead of working with the existing ones
        // that render comments in fqnames
        val renderedFqName = TypeRenderer.renderFqName(relevantDesc)
        val returnTypeFullName = {
          if (isCtor) {
            Constants.void
          } else {
            renderedReturnType(relevantDesc.getOriginal)
          }
        }

        val renderedParameterTypes =
          fnDescriptor.getValueParameters.asScala.toSeq
            .map { valueParam => TypeRenderer.render(valueParam.getType) }
            .mkString(",")
        val signature = returnTypeFullName + "(" + renderedParameterTypes + ")"
        val fullName =
          if (isCtor) {
            s"$renderedFqName${Constants.initPrefix}:$signature"
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

  def fullNameWithSignature(expr: KtBinaryExpression, defaultValue: (String, String)): (String, String) = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        // TODO: write descriptor renderer instead of working with the existing ones
        // that render comments in fqnames
        val renderedParameterTypes =
          fnDescriptor.getValueParameters.asScala.toSeq
            .map { valueParam => TypeRenderer.render(valueParam.getType) }
            .mkString(",")
        val renderedReturnType = TypeRenderer.render(fnDescriptor.getReturnType)
        val signature = renderedReturnType + "(" + renderedParameterTypes + ")"
        val fullName =
          if (fnDescriptor.isInstanceOf[ClassConstructorDescriptorImpl]) {
            s"$renderedReturnType.${Constants.initPrefix}:$signature"
          } else {
            val renderedFqName = TypeRenderer.renderFqName(fnDescriptor)
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

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val decl = fnDescriptor.getContainingDeclaration
        TypeRenderer.renderFqName(decl)
      case None => defaultValue
    }
  }

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind = {
    val callToSuper = expr.getReceiverExpression match {
      case _: KtSuperExpression => true
      case _                    => false
    }

    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val isExtension = DescriptorUtils.isExtension(fnDescriptor)
        val isStatic = DescriptorUtils.isStaticDeclaration(fnDescriptor)
        if (isExtension) CallKinds.ExtensionCall
        else if (isStatic) CallKinds.StaticCall
        else if (callToSuper) CallKinds.StaticCall
        else CallKinds.DynamicCall
      case None => CallKinds.Unknown
    }
  }

  def fullNameWithSignature(expr: KtQualifiedExpression, defaultValue: (String, String)): (String, String) = {
    resolvedCallDescriptor(expr) match {
      case Some(fnDescriptor) =>
        val renderedFqNameForDesc = TypeRenderer.renderFqName(fnDescriptor)
        val renderedFqName =
          if (fnDescriptor.getExtensionReceiverParameter != null) {
            val extType = fnDescriptor.getExtensionReceiverParameter.getType
            val extName = fnDescriptor.getName
            val rendered =
              if (renderedFqNameForDesc.startsWith(Constants.kotlinApplyPrefix)) {
                Constants.kotlinAny
              } else {
                TypeRenderer.render(extType)
              }
            s"$rendered.$extName"
          } else {
            renderedFqNameForDesc
          }
        val valueParameters = fnDescriptor.getValueParameters.asScala.toSeq
        val renderedParameterTypes =
          valueParameters
            .map { valueParam => TypeRenderer.render(valueParam.getType) }
            .mkString(",")
        val bindingInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
        if (bindingInfo != null && bindingInfo.getType != null) {
          val renderedReturnType =
            if (renderedFqNameForDesc.startsWith(Constants.kotlinApplyPrefix)) {
              // TODO: handle `T` in Kotlin stdlib's `apply`
              Constants.kotlinAny
            } else {
              TypeRenderer.render(bindingInfo.getType)
            }
          val signature = s"$renderedReturnType($renderedParameterTypes)"
          val fn = (s"$renderedFqName:$signature", signature)
          fn
        } else {
          defaultValue
        }
      case None => defaultValue
    }
  }

  def parameterType(expr: KtParameter, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
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
    Constants.kotlinAny
  }

  def fullNameWithSignature(expr: KtLambdaExpression, keyPool: KeyPool): (String, String) = {
    val containingFile = expr.getContainingKtFile()
    val fileName = containingFile.getName
    val lambdaNum = keyPool.next
    val astDerivedFullName =
      containingFile.getPackageFqName().toString + ":" + "<lambda>" + "<f_" + fileName + "_no" + lambdaNum + ">" + "()"
    val astDerivedSignature = erasedSignature(expr.getValueParameters().asScala.toList)

    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity == null || mapForEntity.getKeys == null) {
      return (astDerivedFullName, astDerivedSignature)
    }
    val typeInfo = mapForEntity.get(BindingContext.EXPRESSION_TYPE_INFO.getKey)
    val theType = typeInfo.getType
    val constructorDesc = theType.getConstructor.getDeclarationDescriptor
    val constructorType = constructorDesc.getDefaultType
    val args = constructorType.getArguments.asScala.drop(1)
    val renderedArgs =
      if (args.size == 0) {
        ""
      } else if (args.size == 1) {
        Constants.kotlinAny
      } else {
        s"${Constants.kotlinAny}${("," + Constants.kotlinAny) * (args.size - 1)}"
      }
    val signature = Constants.kotlinAny + "(" + renderedArgs + ")"
    val fullName =
      containingFile
        .getPackageFqName()
        .toString + ".<lambda><f_" + fileName + "_no" + lambdaNum.toString + ">" + ":" + signature
    (fullName, signature)
  }

  private def renderedReturnType(fnDesc: FunctionDescriptor): String = {
    val typeParams = fnDesc.getTypeParameters.asScala.toList
    val returnT = fnDesc.getReturnType.getConstructor.getDeclarationDescriptor.getDefaultType
    val typesInTypeParams = typeParams.map(_.getDefaultType.getConstructor.getDeclarationDescriptor.getDefaultType)
    val hasReturnTypeFromTypeParams = typesInTypeParams.contains(returnT)
    if (hasReturnTypeFromTypeParams) {
      if (returnT.getConstructor.getSupertypes.size() > 0) {
        val firstSuperType = returnT.getConstructor.getSupertypes.asScala.toList.head
        TypeRenderer.render(firstSuperType)
      } else {
        val renderedReturnT = TypeRenderer.render(returnT)
        if (renderedReturnT == Constants.tType) {
          Constants.kotlinAny
        } else {
          renderedReturnT
        }
      }
    } else {
      TypeRenderer.render(fnDesc.getReturnType)
    }
  }

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String) = {
    val fnDesc = Option(bindingContext.get(BindingContext.FUNCTION, expr))
    val returnTypeFullName =
      fnDesc match {
        case Some(desc) => renderedReturnType(desc)
        case None       => Constants.any
      }
    val paramTypeNames =
      try {
        val nodeParams = expr.getValueParameters()
        nodeParams.asScala
          .map { p =>
            val explicitTypeFullName = if (p.getTypeReference() != null) {
              p.getTypeReference().getText()
            } else {
              Constants.any
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
          Constants.kotlinAny + "." + expr.getName
        } else {
          val theType = fnDesc.get.getExtensionReceiverParameter.getType
          val renderedType = TypeRenderer.render(theType)
          renderedType + "." + expr.getName
        }
      } else {
        expr.getFqName
      }

    val signature = returnTypeFullName + paramListSignature
    val fullname = s"$methodName:$signature"
    (fullname, signature)
  }

  private def descriptorForNameReference(expr: KtNameReferenceExpression): Option[DeclarationDescriptor] = {
    Option(bindingsForEntity(bindingContext, expr))
      .map(_ => bindingContext.get(BindingContext.REFERENCE_TARGET, expr))
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

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .flatMap {
        case typedDesc: ValueDescriptor =>
          Some(TypeRenderer.render(typedDesc.getType()))
        case typedDesc: WithDefaultType =>
          Some(TypeRenderer.render(typedDesc.getDefaultType()))
        // TODO: add test cases for the LazyClassDescriptors (`okio` codebase serves as good example)
        case typedDesc: LazyClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType()))
        case typedDesc: LazyJavaClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType()))
        case typedDesc: DeserializedClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType()))
        case typedDesc: EnumEntrySyntheticClassDescriptor =>
          Some(TypeRenderer.render(typedDesc.getDefaultType()))
        case unhandled: Any =>
          logger.debug(
            s"Unhandled class type info fetch in `typeFullName[NameReference]` for `${expr.getText}` with class `${unhandled.getClass}`."
          )
          None
        case _ => None
      }
      .getOrElse(defaultValue)
  }

}
