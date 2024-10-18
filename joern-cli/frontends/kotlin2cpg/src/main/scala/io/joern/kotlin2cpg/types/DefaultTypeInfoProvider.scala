package io.joern.kotlin2cpg.types

import io.joern.kotlin2cpg.psi.PsiUtils
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import kotlin.reflect.jvm.internal.impl.load.java.descriptors.JavaClassConstructorDescriptor
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinToJVMBytecodeCompiler
import org.jetbrains.kotlin.cli.jvm.compiler.NoScopeRecordCliBindingTrace
import org.jetbrains.kotlin.com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.ValueDescriptor
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.descriptors.impl.ClassConstructorDescriptorImpl
import org.jetbrains.kotlin.descriptors.impl.EnumEntrySyntheticClassDescriptor
import org.jetbrains.kotlin.descriptors.impl.LazyPackageViewDescriptorImpl
import org.jetbrains.kotlin.descriptors.impl.PropertyDescriptorImpl
import org.jetbrains.kotlin.descriptors.impl.TypeAliasConstructorDescriptorImpl
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaClassDescriptor
import org.jetbrains.kotlin.load.java.sources.JavaSourceElement
import org.jetbrains.kotlin.load.java.structure.impl.classFiles.BinaryJavaMethod
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtArrayAccessExpression
import org.jetbrains.kotlin.psi.KtBinaryExpression
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.psi.KtClassBody
import org.jetbrains.kotlin.psi.KtClassLiteralExpression
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtDestructuringDeclarationEntry
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.psi.KtExpression
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtLambdaExpression
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.KtQualifiedExpression
import org.jetbrains.kotlin.psi.KtSuperExpression
import org.jetbrains.kotlin.psi.KtThisExpression
import org.jetbrains.kotlin.psi.KtTypeAlias
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils.getSuperclassDescriptors
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor
import org.jetbrains.kotlin.types.{KotlinType, TypeUtils}
import org.jetbrains.kotlin.types.error.ErrorType
import org.jetbrains.kotlin.util.slicedMap.ReadOnlySlice
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.control.NonFatal

class DefaultTypeInfoProvider(val bindingContext: BindingContext, typeRenderer: TypeRenderer = new TypeRenderer())
    extends TypeInfoProvider(typeRenderer) {
  private val logger = LoggerFactory.getLogger(getClass)

  import DefaultTypeInfoProvider.*

  private def isValidRender(render: String): Boolean = {
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

  def isStaticMethodCall(expr: KtQualifiedExpression): Boolean = {
    resolvedCallDescriptor(expr)
      .map(_.getSource)
      .collect { case s: JavaSourceElement => s }
      .map(_.getJavaElement)
      .collect { case bjm: BinaryJavaMethod => bjm }
      .exists(_.isStatic)
  }

  def isRefToCompanionObject(expr: KtNameReferenceExpression): Boolean = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity)
      .map(_.getKeys)
      .exists(_.contains(BindingContext.SHORT_REFERENCE_TO_COMPANION_OBJECT.getKey))
  }

  def typeFullName(expr: KtTypeReference, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.TYPE.getKey))
      .map(typeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(defaultValue)
  }

  def propertyType(expr: KtProperty, defaultValue: String): String = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.VARIABLE.getKey))
      .map(_.getType)
      .filterNot(_.isInstanceOf[ErrorType])
      .map(typeRenderer.render(_))
      .filter(isValidRender)
      .getOrElse(
        Option(expr.getTypeReference)
          .map { typeRef =>
            typeFromImports(typeRef.getText, expr.getContainingKtFile).getOrElse(typeRef.getText)
          }
          .getOrElse(defaultValue)
      )
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

  private def isConstructorDescriptor(desc: FunctionDescriptor): Boolean = {
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

  def containingDeclType(expr: KtQualifiedExpression, defaultValue: String): String = {
    resolvedCallDescriptor(expr)
      .map(_.getContainingDeclaration)
      .map(typeRenderer.renderFqNameForDesc)
      .getOrElse(defaultValue)
  }

  def hasStaticDesc(expr: KtQualifiedExpression): Boolean = {
    resolvedCallDescriptor(expr).forall(_.getDispatchReceiverParameter == null)
  }

  def bindingKind(expr: KtQualifiedExpression): CallKind = {
    val isStaticBasedOnStructure = expr.getReceiverExpression.isInstanceOf[KtSuperExpression]
    if (isStaticBasedOnStructure) return CallKind.StaticCall

    val isDynamicBasedOnStructure = expr.getReceiverExpression match {
      case _: KtArrayAccessExpression => true
      case _: KtThisExpression        => true
      case _                          => false
    }
    if (isDynamicBasedOnStructure) return CallKind.DynamicCall

    resolvedCallDescriptor(expr)
      .map { desc =>
        val isExtension = DescriptorUtils.isExtension(desc)
        val isStatic    = DescriptorUtils.isStaticDeclaration(desc) || hasStaticDesc(expr)

        if (isExtension) CallKind.ExtensionCall
        else if (isStatic) CallKind.StaticCall
        else CallKind.DynamicCall
      }
      .getOrElse(CallKind.Unknown)
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
      .collect { case desc: PropertyDescriptorImpl => typeRenderer.renderFqNameForDesc(desc.getContainingDeclaration) }
      .getOrElse(defaultValue)
  }

  def typeFullName(expr: KtNameReferenceExpression, defaultValue: String): String = {
    descriptorForNameReference(expr)
      .flatMap {
        case typedDesc: ValueDescriptor => Some(typeRenderer.render(typedDesc.getType))
        // TODO: add test cases for the LazyClassDescriptors (`okio` codebase serves as good example)
        case typedDesc: LazyClassDescriptor               => Some(typeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: LazyJavaClassDescriptor           => Some(typeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: DeserializedClassDescriptor       => Some(typeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: EnumEntrySyntheticClassDescriptor => Some(typeRenderer.render(typedDesc.getDefaultType))
        case typedDesc: LazyPackageViewDescriptorImpl     => Some(typeRenderer.renderFqNameForDesc(typedDesc))
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
}

object DefaultTypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  def allBindingsOfKind[K, V](bindings: BindingContext, kind: ReadOnlySlice[K, V]): collection.Seq[(K, V)] = {
    val thisField = bindings.getClass.getDeclaredField("this$0")
    thisField.setAccessible(true)
    val bindingTrace = thisField.get(bindings).asInstanceOf[NoScopeRecordCliBindingTrace]

    val mapField = bindingTrace.getClass.getSuperclass.getSuperclass.getDeclaredField("map")
    mapField.setAccessible(true)
    val map = mapField.get(bindingTrace)

    val mapMapField = map.getClass.getDeclaredField("map")
    mapMapField.setAccessible(true)
    val mapMap = mapMapField.get(map).asInstanceOf[java.util.Map[Object, KeyFMap]]

    val result = scala.collection.mutable.ArrayBuffer.empty[(K, V)]

    mapMap.forEach { (keyObject: Object, fMap: KeyFMap) =>
      val kindValue = fMap.get(kind.getKey)
      if (kindValue != null) {
        result.append((keyObject.asInstanceOf[K], kindValue))
      }
    }

    result
  }

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
