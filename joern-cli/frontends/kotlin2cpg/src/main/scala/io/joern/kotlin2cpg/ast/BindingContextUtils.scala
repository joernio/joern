package io.joern.kotlin2cpg.ast

import org.jetbrains.kotlin.cli.jvm.compiler.NoScopeRecordCliBindingTrace
import org.jetbrains.kotlin.com.intellij.util.keyFMap.KeyFMap
import org.jetbrains.kotlin.descriptors.annotations.AnnotationDescriptor
import org.jetbrains.kotlin.descriptors.{
  ClassDescriptor,
  ConstructorDescriptor,
  DeclarationDescriptor,
  FunctionDescriptor,
  TypeAliasDescriptor,
  VariableDescriptor
}
import org.jetbrains.kotlin.psi.{
  Call,
  KtAnnotationEntry,
  KtClassOrObject,
  KtConstructor,
  KtDestructuringDeclarationEntry,
  KtElement,
  KtExpression,
  KtFunctionLiteral,
  KtNamedFunction,
  KtParameter,
  KtProperty,
  KtReferenceExpression,
  KtTypeAlias,
  KtTypeReference
}
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCall
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.error.ErrorType

import scala.jdk.CollectionConverters.*

class BindingContextUtils(val bindingContext: BindingContext) {

  def getClassDesc(classAst: KtClassOrObject): ClassDescriptor = {
    bindingContext.get(BindingContext.CLASS, classAst)
  }

  def getFunctionDesc(functionAst: KtNamedFunction): FunctionDescriptor = {
    bindingContext.get(BindingContext.FUNCTION, functionAst)
  }

  def getFunctionDesc(functionLiteralAst: KtFunctionLiteral): FunctionDescriptor = {
    bindingContext.get(BindingContext.FUNCTION, functionLiteralAst)
  }

  def getConstructorDesc(constructorAst: KtConstructor[?]): ConstructorDescriptor = {
    bindingContext.get(BindingContext.CONSTRUCTOR, constructorAst)
  }

  def getCalledFunctionDesc(destructuringAst: KtDestructuringDeclarationEntry): Option[FunctionDescriptor] = {
    val resolvedCall = Option(bindingContext.get(BindingContext.COMPONENT_RESOLVED_CALL, destructuringAst))
    resolvedCall.map(_.getResultingDescriptor)
  }

  def getCalledFunctionDesc(expressionAst: KtExpression): Option[FunctionDescriptor] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expressionAst))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))
    resolvedCall.map(_.getResultingDescriptor).collect { case functionDesc: FunctionDescriptor => functionDesc }
  }

  def getAmbiguousCalledFunctionDescs(expression: KtExpression): collection.Seq[FunctionDescriptor] = {
    val descriptors = bindingContext.get(BindingContext.AMBIGUOUS_REFERENCE_TARGET, expression)
    if (descriptors == null) { return Seq.empty }
    descriptors.asScala.toSeq.collect { case functionDescriptor: FunctionDescriptor => functionDescriptor }
  }

  def getResolvedCallDesc(expr: KtExpression): Option[ResolvedCall[?]] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expr))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))
    resolvedCall
  }

  def getVariableDesc(param: KtParameter): Option[VariableDescriptor] = {
    Option(bindingContext.get(BindingContext.VALUE_PARAMETER, param))
  }

  def getVariableDesc(entry: KtDestructuringDeclarationEntry): Option[VariableDescriptor] = {
    Option(bindingContext.get(BindingContext.VARIABLE, entry))
  }

  def getVariableDesc(property: KtProperty): Option[VariableDescriptor] = {
    Option(bindingContext.get(BindingContext.VARIABLE, property))
  }

  def getTypeAliasDesc(typeAlias: KtTypeAlias): TypeAliasDescriptor = {
    bindingContext.get(BindingContext.TYPE_ALIAS, typeAlias)
  }

  def getAnnotationDesc(entry: KtAnnotationEntry): AnnotationDescriptor = {
    bindingContext.get(BindingContext.ANNOTATION, entry)
  }

  def getDeclDesc(nameRefExpr: KtReferenceExpression): Option[DeclarationDescriptor] = {
    Option(bindingContext.get(BindingContext.REFERENCE_TARGET, nameRefExpr))
  }

  def getExprType(expr: KtExpression): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .flatMap(typeInfo => Option(typeInfo.getType))
  }

  def getExpectedExprType(expr: KtExpression): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.EXPECTED_EXPRESSION_TYPE, expr))
  }

  def getTypeRefType(typeRef: KtTypeReference): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.TYPE, typeRef)) match {
      case Some(_: ErrorType) => None
      case other              => other
    }
  }

  // This function is here for debug purposes.
  // It shows all bindings associated with a certain entity.
  def debugBindingsForEntity(entity: KtElement | Call): KeyFMap = {
    try {
      val thisField = bindingContext.getClass.getDeclaredField("this$0")
      thisField.setAccessible(true)
      val bindingTrace = thisField.get(bindingContext).asInstanceOf[NoScopeRecordCliBindingTrace]

      val mapField = bindingTrace.getClass.getSuperclass.getSuperclass.getDeclaredField("map")
      mapField.setAccessible(true)
      val map = mapField.get(bindingTrace)

      val mapMapField = map.getClass.getDeclaredField("map")
      mapMapField.setAccessible(true)
      val mapMap = mapMapField.get(map).asInstanceOf[java.util.Map[Object, KeyFMap]]

      val mapForEntity = mapMap.get(entity)
      mapForEntity
    } catch {
      case _ =>
        KeyFMap.EMPTY_MAP
    }
  }
}
