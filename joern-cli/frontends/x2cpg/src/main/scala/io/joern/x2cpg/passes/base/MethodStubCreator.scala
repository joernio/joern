package io.joern.x2cpg.passes.base

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.base.MethodStubCreator.createMethodStub
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import scala.collection.mutable
import scala.util.Try

case class CallSummary(name: String, signature: String, fullName: String, dispatchType: String)

/** This pass has no other pass as prerequisite.
  */
class MethodStubCreator(cpg: Cpg) extends CpgPass(cpg) {

  // Since the method fullNames for fuzzyc are not unique, we do not have
  // a 1to1 relation and may overwrite some values. This is ok for now.
  private val methodFullNameToNode   = mutable.LinkedHashMap[String, Method]()
  private val methodToParameterCount = mutable.LinkedHashMap[CallSummary, Int]()

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    for (method <- cpg.method) {
      methodFullNameToNode.put(method.fullName, method)
    }

    for (call <- cpg.call if call.methodFullName != Defines.DynamicCallUnknownFullName) {
      methodToParameterCount.put(
        CallSummary(call.name, call.signature, call.methodFullName, call.dispatchType),
        call.argument.size
      )
    }

    for (
      (CallSummary(name, signature, fullName, dispatchType), parameterCount) <- methodToParameterCount
      if !methodFullNameToNode.contains(fullName)
    ) {
      createMethodStub(name, fullName, signature, dispatchType, parameterCount, dstGraph)
    }
  }

  override def finish(): Unit = {
    methodFullNameToNode.clear()
    methodToParameterCount.clear()
    super.finish()
  }

}

object MethodStubCreator {

  private def addLineNumberInfo(methodNode: NewMethod, fullName: String): NewMethod = {
    val s = fullName.split(":")
    if (
      s.size == 5 && Try {
        s(1).toInt
      }.isSuccess && Try {
        s(2).toInt
      }.isSuccess
    ) {
      val filename      = s(0)
      val lineNumber    = s(1).toInt
      val lineNumberEnd = s(2).toInt
      methodNode
        .filename(filename)
        .lineNumber(lineNumber)
        .lineNumberEnd(lineNumberEnd)
    } else {
      methodNode
    }
  }

  def createMethodStub(
    name: String,
    fullName: String,
    signature: String,
    dispatchType: String,
    parameterCount: Int,
    dstGraph: DiffGraphBuilder,
    isExternal: Boolean = true,
    astParentType: String = NodeTypes.NAMESPACE_BLOCK,
    astParentFullName: String = "<global>"
  ): NewMethod = {
    val methodNode = NewMethod()
      .name(name)
      .fullName(fullName)
      .isExternal(isExternal)
      .signature(signature)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .order(0)

    addLineNumberInfo(methodNode, fullName)

    dstGraph.addNode(methodNode)

    val firstParameterOrder = dispatchType match {
      case DispatchTypes.DYNAMIC_DISPATCH => 0
      case _                              => 1
    }

    (firstParameterOrder to parameterCount).zipWithIndex.foreach { case (parameterOrder, index) =>
      val nameAndCode = s"p$parameterOrder"
      val param = NewMethodParameterIn()
        .code(nameAndCode)
        .order(parameterOrder)
        .index(index + 1)
        .name(nameAndCode)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName("ANY")

      dstGraph.addNode(param)
      dstGraph.addEdge(methodNode, param, EdgeTypes.AST)
    }

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    dstGraph.addNode(blockNode)
    dstGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)

    val methodReturn = NewMethodReturn()
      .order(2)
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")

    dstGraph.addNode(methodReturn)
    dstGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)

    methodNode
  }
}
