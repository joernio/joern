package io.joern.kotlin2cpg.types

import kotlin.reflect.jvm.internal.impl.load.java.descriptors.JavaClassConstructorDescriptor
import org.jetbrains.kotlin.cli.jvm.compiler.NoScopeRecordCliBindingTrace
import org.jetbrains.kotlin.com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.impl.ClassConstructorDescriptorImpl
import org.jetbrains.kotlin.descriptors.impl.TypeAliasConstructorDescriptorImpl
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaClassDescriptor
import org.jetbrains.kotlin.load.java.sources.JavaSourceElement
import org.jetbrains.kotlin.load.java.structure.impl.classFiles.BinaryJavaMethod
import org.jetbrains.kotlin.psi.{
  Call,
  KtArrayAccessExpression,
  KtBinaryExpression,
  KtCallExpression,
  KtElement,
  KtExpression,
  KtNameReferenceExpression,
  KtQualifiedExpression,
  KtSuperExpression,
  KtThisExpression
}
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.util.slicedMap.ReadOnlySlice
import org.slf4j.LoggerFactory

import scala.annotation.unused
import scala.util.control.NonFatal

class TypeInfoProvider(val bindingContext: BindingContext) {

  import io.joern.kotlin2cpg.types.TypeInfoProvider.bindingsForEntity

  def usedAsExpression(expr: KtExpression): Option[Boolean] = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    Option(mapForEntity.get(BindingContext.USED_AS_EXPRESSION.getKey)).map(_.booleanValue())
  }

  def usedAsImplicitThis(expr: KtNameReferenceExpression): Boolean = {
    val mapForEntity = bindingsForEntity(bindingContext, expr)
    val isCallExprWithTarget = Option(mapForEntity)
      .map(_.getKeys)
      .exists(ks =>
        ks.contains(BindingContext.CALL.getKey)
          && ks.contains(BindingContext.USED_AS_EXPRESSION.getKey)
          && ks.contains(BindingContext.REFERENCE_TARGET.getKey)
      )
    isCallExprWithTarget && resolvedPropertyDescriptor(expr).exists { d =>
      d.getDispatchReceiverParameter != null && d.getDispatchReceiverParameter.getName.asString() == "<this>"
    }
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

  private def resolvedPropertyDescriptor(expr: KtNameReferenceExpression): Option[PropertyDescriptor] = {
    val descMaybe = for {
      callForSubexpression <- Option(bindingContext.get(BindingContext.REFERENCE_TARGET, expr))
      desc = callForSubexpression
    } yield desc
    descMaybe.collect { case desc: PropertyDescriptor => desc }
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

  private def hasStaticDesc(expr: KtQualifiedExpression): Boolean = {
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
}

object TypeInfoProvider {
  private val logger = LoggerFactory.getLogger(getClass)

  /** For internal debugging purposes */
  @unused
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

  /** For internal debugging purposes */
  def bindingsForEntity(bindings: BindingContext, entity: KtElement | Call): KeyFMap = {
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
        logger.debug(s"Encountered _no such field_ exception while retrieving type info for `$entity`: `$noSuchField`.")
        KeyFMap.EMPTY_MAP
      case e if NonFatal(e) =>
        logger.debug(s"Encountered general exception while retrieving type info for `$entity`: `$e`.")
        KeyFMap.EMPTY_MAP
    }
  }

  /** For internal debugging purposes */
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

  @unused
  def printBindingsForEntity(bindings: BindingContext, entity: KtElement): Unit = {
    println(bindingsForEntityAsString(bindings, entity))
  }
}
