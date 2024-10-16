package io.joern.kotlin2cpg.ast

import org.jetbrains.kotlin.descriptors.{ClassDescriptor, ConstructorDescriptor, FunctionDescriptor, VariableDescriptor}
import org.jetbrains.kotlin.psi.{
  KtClassOrObject,
  KtConstructor,
  KtDestructuringDeclarationEntry,
  KtExpression,
  KtFunctionLiteral,
  KtNamedFunction,
  KtParameter
}
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCall
import org.jetbrains.kotlin.types.KotlinType

import scala.jdk.CollectionConverters.*

class BindingContextUtils(bindingContext: BindingContext) {
  def getClassDesc(classAst: KtClassOrObject): Option[ClassDescriptor] = {
    Option(bindingContext.get(BindingContext.CLASS, classAst))
  }

  def getFunctionDesc(functionAst: KtNamedFunction): Option[FunctionDescriptor] = {
    Option(bindingContext.get(BindingContext.FUNCTION, functionAst))
  }

  def getFunctionDesc(functionLiteralAst: KtFunctionLiteral): Option[FunctionDescriptor] = {
    Option(bindingContext.get(BindingContext.FUNCTION, functionLiteralAst))
  }

  def getFunctionDesc(destructuringAst: KtDestructuringDeclarationEntry): Option[FunctionDescriptor] = {
    val resolvedCall = Option(bindingContext.get(BindingContext.COMPONENT_RESOLVED_CALL, destructuringAst))

    resolvedCall.map(_.getResultingDescriptor).collect { case functionDesc: FunctionDescriptor =>
      functionDesc
    }
  }

  def getConstructorDesc(constructorAst: KtConstructor[?]): Option[ConstructorDescriptor] = {
    Option(bindingContext.get(BindingContext.CONSTRUCTOR, constructorAst))
  }

  def getCalledFunctionDesc(expressionAst: KtExpression): Option[FunctionDescriptor] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expressionAst))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))

    resolvedCall.map(_.getResultingDescriptor).collect { case functionDesc: FunctionDescriptor =>
      functionDesc
    }
  }

  def getAmbiguousCalledFunctionDescs(expression: KtExpression): collection.Seq[FunctionDescriptor] = {
    val descriptors = bindingContext.get(BindingContext.AMBIGUOUS_REFERENCE_TARGET, expression)
    if (descriptors == null) {
      return Seq.empty
    }

    descriptors.asScala.toSeq.collect { case functionDescriptor: FunctionDescriptor =>
      functionDescriptor
    }

  }

  def getResolvedCallDesc(expr: KtExpression): Option[ResolvedCall[?]] = {
    val call         = Option(bindingContext.get(BindingContext.CALL, expr))
    val resolvedCall = call.flatMap(call => Option(bindingContext.get(BindingContext.RESOLVED_CALL, call)))

    resolvedCall
  }

  def getVariableDesc(param: KtParameter): Option[VariableDescriptor] = {
    Option(bindingContext.get(BindingContext.VALUE_PARAMETER, param))
  }

  def getExprType(expr: KtExpression): Option[KotlinType] = {
    Option(bindingContext.get(BindingContext.EXPRESSION_TYPE_INFO, expr))
      .flatMap(typeInfo => Option(typeInfo.getType))
  }
}
