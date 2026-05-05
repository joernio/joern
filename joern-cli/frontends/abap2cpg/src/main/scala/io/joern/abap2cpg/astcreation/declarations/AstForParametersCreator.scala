package io.joern.abap2cpg.astcreation.declarations

import io.joern.abap2cpg.astcreation.AstHelpers
import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Methods for creating parameter and local variable nodes */
trait AstForParametersCreator { this: AstCreator & AstHelpers =>

  /** Create METHOD_PARAMETER_IN node */
  protected def createParameterIn(param: Parameter, index: Int): NewMethodParameterIn = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterIn()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_PARAMETER_OUT node */
  protected def createParameterOut(param: Parameter, index: Int): NewMethodParameterOut = {
    val evaluationStrategy = if (param.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodParameterOut()
      .name(param.name)
      .code(param.name)
      .typeFullName(param.typeName)
      .evaluationStrategy(evaluationStrategy)
      .index(index)
      .order(index)
  }

  /** Create METHOD_RETURN node */
  protected def createMethodReturn(returnParam: Parameter): NewMethodReturn = {
    val evaluationStrategy = if (returnParam.isValue) {
      EvaluationStrategies.BY_VALUE
    } else {
      EvaluationStrategies.BY_REFERENCE
    }

    NewMethodReturn()
      .evaluationStrategy(evaluationStrategy)
      .typeFullName(returnParam.typeName)
      .code(returnParam.name)
  }

  /** Create a local variable declaration */
  protected def createLocal(dataDecl: DataDeclaration, order: Int): NewLocal = {
    val codeStr = dataDecl.initialValue match {
      case Some(initVal) =>
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName} VALUE ${codeFromExpr(initVal)}."
      case None =>
        s"DATA: ${dataDecl.name} TYPE ${dataDecl.typeName}."
    }

    val localNode = NewLocal()
      .name(dataDecl.name)
      .code(codeStr)
      .typeFullName(dataDecl.typeName)
      .order(order)

    dataDecl.span.start.foreach { pos =>
      localNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    localNode
  }
}
