package io.joern.x2cpg.passes.base

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.base.MethodStubCreator.{createMethodStub, linkToTypeDecl}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.util.{Success, Try}

case class CallSummary(name: String, signature: String, fullName: String, dispatchType: String)

/** This pass has no other pass as prerequisite.
  */
class MethodStubCreator(cpg: Cpg) extends CpgPass(cpg) {

  // Since the method fullNames for fuzzyc are not unique, we do not have
  // a 1to1 relation and may overwrite some values. This is ok for now.
  private val methodFullNameToNode   = mutable.LinkedHashMap[String, Method]()
  private val methodToParameterCount = mutable.LinkedHashMap[CallSummary, Int]()

  override def run(dstGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    for (method <- cpg.method) {
      methodFullNameToNode.put(method.fullName, method)
    }

    for (call <- cpg.call if call.methodFullName != Defines.DynamicCallUnknownFallName) {
      methodToParameterCount.put(
        CallSummary(call.name, call.signature, call.methodFullName, call.dispatchType),
        call.argument.size
      )
    }

    for (
      (CallSummary(name, signature, fullName, dispatchType), parameterCount) <- methodToParameterCount
      if !methodFullNameToNode.contains(fullName)
    ) {
      val stub = createMethodStub(name, fullName, signature, dispatchType, parameterCount, dstGraph)
      linkToTypeDecl(cpg, stub, dstGraph)
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

  /** Will attempt to link the method stub to a type declaration if one exists. This uses the `name` and `fullName`
    * properties of the stub to determine the type declaration.
    *
    * Method full names take 2 designs in the CPG. This approach works for both.
    *
    * 1: Dynamic languages do `filename`:`some_path`.`call_name`.`optional_info`
    *
    * 2: Static languages do `namespace`.`type_name`.`call_name`:`signature`
    *
    * @param cpg
    *   the code property graph, containing the potential type declaration.
    * @param stub
    *   the method stub.
    * @param dstGraph
    *   the graph builder.
    * @return
    *   the type declaration that is linked, if found.
    */
  def linkToTypeDecl(cpg: Cpg, stub: NewMethod, dstGraph: DiffGraphBuilder): Option[TypeDecl] = {
    Try {
      val nameIdx = stub.fullName.indexOf(stub.name)
      stub.fullName.substring(0, nameIdx - 1)
    } match {
      case Success(typeFullName) if !typeFullName.isBlank && !typeFullName.startsWith("<operator>") =>
        cpg.typeDecl
          .fullNameExact(typeFullName)
          .map { t => dstGraph.addEdge(t, stub, EdgeTypes.AST); t }
          .headOption
      case _ => None
    }
  }

  def createMethodStub(
    name: String,
    fullName: String,
    signature: String,
    dispatchType: String,
    parameterCount: Int,
    dstGraph: DiffGraphBuilder,
    isExternal: Boolean = true
  ): NewMethod = {
    val methodNode = addLineNumberInfo(
      NewMethod()
        .name(name)
        .fullName(fullName)
        .isExternal(isExternal)
        .signature(signature)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName("<global>")
        .order(0),
      fullName
    )

    dstGraph.addNode(methodNode)

    val firstParameterIndex = dispatchType match {
      case DispatchTypes.DYNAMIC_DISPATCH =>
        0
      case _ =>
        1
    }

    (firstParameterIndex to parameterCount).foreach { parameterOrder =>
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
