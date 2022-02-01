package io.joern.kotlin2cpg.types

import com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.{
  DeclarationDescriptor,
  FunctionDescriptor,
  ValueDescriptor,
  ValueParameterDescriptor
}
import org.jetbrains.kotlin.descriptors.impl.{
  ClassConstructorDescriptorImpl,
  EnumEntrySyntheticClassDescriptor,
  LazyPackageViewDescriptorImpl,
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
  KtTypeAlias
}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.{
  LazyClassDescriptor,
  LazyPackageDescriptor,
  LazyTypeAliasDescriptor
}
import org.jetbrains.kotlin.types.{
  ErrorType,
  KotlinType,
  KotlinTypeFactoryKt,
  KotlinTypeKt,
  SimpleType,
  TypeUtils,
  UnresolvedType
}
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
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.resolve.`lazy`.NoDescriptorForDeclarationException
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor

// representative of `LazyJavaClassDescriptor`, `DeserializedClassDescriptor`, `TypeAliasConstructorDescriptor`, etc.
trait WithDefaultType {
  def getDefaultType(): SimpleType
}

object Constants {
  val kotlinAny = "kotlin.Any"
  val kotlinString = "kotlin.String"
  val any = "ANY"
  val void = "void"
  val classLiteralReplacementMethodName = "getClass"
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
  def descriptorRenderer(desc: DeclarationDescriptor): DescriptorRenderer = {
    val opts = new DescriptorRendererOptionsImpl
    opts.setParameterNamesInFunctionalTypes(false)
    if (desc != null) {
      val anyT = DescriptorUtilsKt.getBuiltIns(desc).getAny()
      opts.setTypeNormalizer { t =>
        t match {
          case _: UnresolvedType => anyT.getDefaultType
          case _: ErrorType      => anyT.getDefaultType
          case _                 => t
        }
      }
    }
    new DescriptorRendererImpl(opts)
  }

  def renderFqName(desc: DeclarationDescriptor): String = {
    val renderer = descriptorRenderer(desc)
    val fqName = DescriptorUtils.getFqName(desc)
    stripped(renderer.renderFqName(fqName))
  }

  def render(t: KotlinType): String = {
    val renderer = descriptorRenderer(t.getConstructor.getDeclarationDescriptor)
    if (isFunctionXType(t)) {
      Constants.kotlinFunctionXPrefix + (t.getArguments.size() - 1).toString
    } else {
      val rendered = renderer.renderType(t)
      stripped(rendered)
    }
  }

  private def isFunctionXType(t: KotlinType): Boolean = {
    val renderer = DescriptorRenderer.FQ_NAMES_IN_TYPES
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

  // TODO: consider the psiFactory fns like `createExpression` for
  // adding information to the PSI graph which would not be there otherwise
  /*
  lazy val psiFactory = {
    new KtPsiFactory(environment.getProject)
  }
   */

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

  def fullName(expr: KtTypeAlias, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.TYPE_ALIAS.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.TYPE_ALIAS.getKey)
      if (variableDesc == null) {
        return or
      }
      val rendered = TypeRenderer.renderFqName(variableDesc)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def aliasTypeFullName(expr: KtTypeAlias, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.TYPE_ALIAS.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.TYPE_ALIAS.getKey)
      if (variableDesc == null) {
        return or
      }
      val desc = variableDesc.asInstanceOf[LazyTypeAliasDescriptor]
      val rendered = TypeRenderer.render(desc.getUnderlyingType)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def returnType(expr: KtNamedFunction, or: String): String = {
    val bindingInfo = bindingContext.get(BindingContext.FUNCTION, expr)
    if (bindingInfo != null) {
      val rendered = TypeRenderer.render(bindingInfo.getReturnType)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def propertyType(expr: KtProperty, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.VARIABLE.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.VARIABLE.getKey)
      val rendered = TypeRenderer.render(variableDesc.getType)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def inheritanceTypes(expr: KtClassOrObject, or: Seq[String]): Seq[String] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    val descVal = mapForEntity.get(BindingContext.CLASS.getKey)
    if (descVal == null) {
      return or
    }
    val desc = descVal.asInstanceOf[LazyClassDescriptor]
    val superClassDescriptors = getSuperclassDescriptors(desc)
    if (superClassDescriptors.size() > 0) {
      superClassDescriptors.asScala.map { superClassDesc =>
        TypeRenderer.render(superClassDesc.getDefaultType)
      }.toList
    } else {
      or
    }
  }

  def fullName(expr: KtClassOrObject, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    if (mapForEntity.getKeys.contains(BindingContext.CLASS.getKey)) {
      val desc = mapForEntity.get(BindingContext.CLASS.getKey)
      val rendered = TypeRenderer.render(desc.getDefaultType)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def expressionType(expr: KtExpression, or: String): String = {
    val typeInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (typeInfo != null && typeInfo.getType != null) {
      val rendered = TypeRenderer.render(typeInfo.getType)
      if (isValidRender(rendered)) {
        rendered
      } else {
        or
      }
    } else {
      or
    }
  }

  def fullNameWithSignature(expr: KtClassLiteralExpression, or: (String, String)): (String, String) = {
    val typeInfo = bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr)
    if (typeInfo != null && typeInfo.getType != null && typeInfo.getType.getArguments.size() > 0) {
      val firstTypeArg = typeInfo.getType.getArguments.get(0)
      val rendered = TypeRenderer.render(firstTypeArg.getType)
      val retType = expressionType(expr, Constants.any)
      val signature = retType + "()"
      val fullName = rendered + "." + Constants.classLiteralReplacementMethodName + ":" + signature
      (fullName, signature)
    } else {
      (or._1, or._2)
    }
  }

  private def subexpressionForResolvedCallInfo(expr: KtExpression): KtExpression = {
    expr match {
      case typedExpr: KtCallExpression =>
        val firstChild = typedExpr.getFirstChild
        if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
          firstChild.asInstanceOf[KtExpression]
        } else {
          expr
        }
      case typedExpr: KtQualifiedExpression =>
        val selectorExpr = typedExpr.getSelectorExpression
        selectorExpr match {
          case call: KtCallExpression =>
            val firstChild = call.getFirstChild
            if (firstChild != null && firstChild.isInstanceOf[KtExpression]) {
              firstChild.asInstanceOf[KtExpression]
            } else {
              expr
            }
          case _ => typedExpr
        }
      case typedExpr: KtBinaryExpression =>
        val secondChild = typedExpr.getChildren.toList(1)
        if (secondChild != null && secondChild.isInstanceOf[KtExpression]) {
          secondChild.asInstanceOf[KtExpression]
        } else {
          expr
        }
      case _ => expr
    }
  }

  private def resolvedCallDescriptor(expr: KtExpression): Option[FunctionDescriptor] = {
    val relevantSubexpression = subexpressionForResolvedCallInfo(expr)
    val callForSubexpression = Option(bindingContext.get(BindingContext.CALL, relevantSubexpression))
    if (!callForSubexpression.isDefined) {
      None
    } else {
      val resolvedCallForSubexpression = Option(
        bindingContext.get(BindingContext.RESOLVED_CALL, callForSubexpression.get)
      )
      if (!resolvedCallForSubexpression.isDefined) {
        None
      } else {
        resolvedCallForSubexpression.get.getResultingDescriptor match {
          case desc: FunctionDescriptor =>
            Some(desc)
          case _ =>
            None
        }
      }
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

  def fullNameWithSignature(expr: KtCallExpression, or: (String, String)): (String, String) = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val isCtor = fnDescriptor match {
          case _: ClassConstructorDescriptorImpl     => true
          case _: TypeAliasConstructorDescriptorImpl => true
          case _                                     => false
        }

        // TODO: write descriptor renderer instead of working with the existing ones
        // that render comments in fqnames
        val renderedFqName = TypeRenderer.renderFqName(fnDescriptor)
        val renderedReturnType =
          if (isCtor) {
            Constants.void
          } else {
            TypeRenderer.render(fnDescriptor.getReturnType)
          }

        val renderedParameterTypes =
          fnDescriptor.getValueParameters.asScala.toSeq
            .map { valueParam => TypeRenderer.render(valueParam.getType) }
            .mkString(",")
        val signature = renderedReturnType + "(" + renderedParameterTypes + ")"
        val fullName =
          if (isCtor) {
            renderedFqName + Constants.initPrefix + ":" + signature
          } else {
            renderedFqName + ":" + signature
          }

        if (!isValidRender(fullName) || !isValidRender(signature)) {
          (or._1, or._2)
        } else {
          (fullName, signature)
        }
      case None =>
        (or._1, or._2)
    }
  }

  def fullNameWithSignature(expr: KtBinaryExpression, or: (String, String)): (String, String) = {
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
            renderedReturnType + "." + Constants.initPrefix + ":" + signature
          } else {
            val renderedFqName = TypeRenderer.renderFqName(fnDescriptor)
            renderedFqName + ":" + signature
          }
        if (!isValidRender(fullName) || !isValidRender(signature)) {
          (or._1, or._2)
        } else {
          (fullName, signature)
        }
      case None => or
    }
  }

  def containingDeclType(expr: KtQualifiedExpression, or: String): String = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val decl = fnDescriptor.getContainingDeclaration
        TypeRenderer.renderFqName(decl)
      case None => or
    }
  }

  def bindingKind(expr: KtQualifiedExpression): CallKinds.CallKind = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
      case Some(fnDescriptor) =>
        val isExtension = DescriptorUtils.isExtension(fnDescriptor)
        val isStatic = DescriptorUtils.isStaticDeclaration(fnDescriptor)
        if (isExtension) CallKinds.ExtensionCall
        else if (isStatic) CallKinds.StaticCall
        else CallKinds.DynamicCall
      case None => CallKinds.Unknown
    }
  }

  def fullNameWithSignature(
      expr: KtQualifiedExpression,
      or: (String, String)
  ): (String, String) = {
    val resolvedDesc = resolvedCallDescriptor(expr)
    resolvedDesc match {
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
            rendered + "." + extName
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
          val signature = renderedReturnType + "(" + renderedParameterTypes + ")"
          val fn = (renderedFqName + ":" + signature, signature)
          fn
        } else {
          or
        }
      case None => or
    }
  }

  def parameterType(expr: KtParameter, or: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    // TODO: add specific test for no binding info of parameter
    // triggered by exception in https://github.com/agrosner/DBFlow
    // TODO: ...also test cases for non-null binding info for other fns
    if (mapForEntity == null || mapForEntity.getKeys == null) {
      return or
    }
    if (mapForEntity.getKeys.contains(BindingContext.VALUE_PARAMETER.getKey)) {
      val variableDesc = mapForEntity.get(BindingContext.VALUE_PARAMETER.getKey)
      val render = TypeRenderer.render(variableDesc.getType)
      if (isValidRender(render)) {
        render
      } else {
        or
      }
    } else {
      or
    }
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
        Constants.kotlinAny + ("," + Constants.kotlinAny) * (args.size - 1)
      }
    val signature = Constants.kotlinAny + "(" + renderedArgs + ")"
    val fullName =
      containingFile
        .getPackageFqName()
        .toString + ".<lambda><f_" + fileName + "_no" + lambdaNum.toString + ">" + ":" + signature
    (fullName, signature)
  }

  def fullNameWithSignature(expr: KtNamedFunction, or: (String, String)): (String, String) = {
    val returnTypeFullName = returnType(expr, Constants.any)

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
    val paramListSignature = "(" + paramTypeNames.mkString(",") + ")"

    val bindingInfo = bindingContext.get(BindingContext.FUNCTION, expr)
    val methodName =
      if (bindingInfo != null && bindingInfo.getExtensionReceiverParameter != null) {
        val erpType = bindingInfo.getExtensionReceiverParameter.getType
        if (erpType.isInstanceOf[UnresolvedType]) {
          Constants.kotlinAny + "." + expr.getName
        } else {
          val theType = bindingInfo.getExtensionReceiverParameter.getType
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
    val mapForEntity = Option(bindingsForEntity(bindingContext, expr))
    if (mapForEntity.isDefined) {
      Option(bindingContext.get(BindingContext.REFERENCE_TARGET, expr))
    } else {
      None
    }
  }

  def nameReferenceKind(expr: KtNameReferenceExpression): NameReferenceKinds.NameReferenceKind = {
    val targetDesc = descriptorForNameReference(expr)
    if (targetDesc.isDefined) {
      targetDesc.get match {
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
    } else {
      NameReferenceKinds.Unknown
    }
  }

  def typeFullName(expr: KtNameReferenceExpression, or: String): String = {
    val targetDesc = descriptorForNameReference(expr)
    if (targetDesc.isDefined) {
      val rendered =
        targetDesc.get match {
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
        }
      rendered match {
        case Some(r) => r
        case None    => or
      }
    } else or
  }
}
