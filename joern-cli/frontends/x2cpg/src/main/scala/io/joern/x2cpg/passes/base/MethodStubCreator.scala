package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import scala.collection.mutable
import scala.util.Try

case class NameAndSignature(name: String, signature: String, fullName: String)

/** This pass has no other pass as prerequisite.
  */
class MethodStubCreator(cpg: Cpg) extends SimpleCpgPass(cpg) {

  // Since the method fullNames for fuzzyc are not unique, we do not have
  // a 1to1 relation and may overwrite some values. This is ok for now.
  private val methodFullNameToNode   = mutable.LinkedHashMap[String, Method]()
  private val methodToParameterCount = mutable.LinkedHashMap[NameAndSignature, Int]()

  override def run(dstGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    for (method <- cpg.method) {
      methodFullNameToNode.put(method.fullName, method)
    }

    for (call <- cpg.call) {
      methodToParameterCount.put(NameAndSignature(call.name, call.signature, call.methodFullName), call.argument.size)
    }

    for (
      (NameAndSignature(name, signature, fullName), parameterCount) <- methodToParameterCount
      if !methodFullNameToNode.contains(fullName)
    ) {
      createMethodStub(name, fullName, signature, parameterCount, dstGraph)
    }
  }

  override def finish(): Unit = {
    methodFullNameToNode.clear()
    methodToParameterCount.clear()
    super.finish()
  }

  private def addLineNumberInfo(methodNode: NewMethod, fullName: String): NewMethod = {
    val s = fullName.split(":")
    if (s.size == 5 && Try { s(1).toInt }.isSuccess && Try { s(2).toInt }.isSuccess) {
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

  private def createMethodStub(
    name: String,
    fullName: String,
    signature: String,
    parameterCount: Int,
    dstGraph: DiffGraphBuilder
  ): NewMethod = {
    val methodNode = addLineNumberInfo(
      NewMethod()
        .name(name)
        .fullName(fullName)
        .isExternal(true)
        .signature(signature)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName("<global>")
        .order(0),
      fullName
    )

    dstGraph.addNode(methodNode)

    (1 to parameterCount).foreach { parameterOrder =>
      val nameAndCode = s"p$parameterOrder"
      val param = NewMethodParameterIn()
        .code(nameAndCode)
        .order(parameterOrder)
        .name(nameAndCode)
        .evaluationStrategy(EvaluationStrategies.BY_VALUE)
        .typeFullName("ANY")

      dstGraph.addNode(param)
      dstGraph.addEdge(methodNode, param, EdgeTypes.AST)
    }

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")

    dstGraph.addNode(methodReturn)
    dstGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    dstGraph.addNode(blockNode)
    dstGraph.addEdge(methodNode, blockNode, EdgeTypes.AST)

    methodNode
  }

}
