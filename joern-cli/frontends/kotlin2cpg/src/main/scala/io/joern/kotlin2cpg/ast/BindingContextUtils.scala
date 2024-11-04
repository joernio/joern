package io.joern.kotlin2cpg.ast

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
  KtAnnotationEntry,
  KtClassOrObject,
  KtConstructor,
  KtDestructuringDeclarationEntry,
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

  def getTypeRefType(typeRef: KtTypeReference): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.TYPE, typeRef)) match {
      case Some(_: ErrorType) => None
      case other              => other
    }
  }
}
