package io.joern.x2cpg.passes.base

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.base.MethodStubCreator.{createMethodStub, logger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import scala.collection.mutable
import scala.util.Try

case class CallSummary(
  name: String,
  signature: String,
  fullName: String,
  dispatchType: String,
  minArg: Int,
  maxArg: Int,
  numArg: Int
)

/** This pass has no other pass as prerequisite.
  */
class MethodStubCreator(cpg: Cpg) extends CpgPass(cpg) {

  // Since the method fullNames for fuzzyc are not unique, we do not have
  // a 1to1 relation and may overwrite some values. This is ok for now.
  // NEWC has unique method fullNames now, so we can use this to create the stubs anyway.
  private val methodToParameterCount = mutable.LinkedHashMap[String, mutable.LinkedHashSet[CallSummary]]()

  override def run(dstGraph: DiffGraphBuilder): Unit = {

    for (
      call <- cpg.call if call.methodFullName != Defines.DynamicCallUnknownFullName && cpg.method
        .fullNameExact(call.methodFullName)
        .isEmpty
    ) {
      methodToParameterCount
        .getOrElseUpdate(call.methodFullName, mutable.LinkedHashSet.empty[CallSummary])
        .add(
          CallSummary(
            call.name,
            call.signature,
            call.methodFullName,
            call.dispatchType,
            call.argument.argumentIndex.minOption.getOrElse(0),
            call.argument.argumentIndex.maxOption.getOrElse(0),
            call.argument.size
          )
        )
    }
    for ((fullName, callSummaries) <- methodToParameterCount) {
      var done = false
      if (callSummaries.size == 1) {
        val callSummary = callSummaries.head
        if (
          callSummary.numArg == callSummary.maxArg - callSummary.minArg + 1 &&
          (callSummary.minArg == 0 || callSummary.minArg == 1)
        ) {
          done = true
          createMethodStub(
            callSummary.name,
            callSummary.fullName,
            callSummary.signature,
            callSummary.dispatchType,
            callSummary.numArg,
            dstGraph,
            startWithInst = Some(callSummary.minArg == 0)
          )
        }
      }
      if (!done) {
        // something is broken :(
        logger.info(
          s"Inconsistent/erroneous callInfo on calls to method fullname $fullName (we have ${callSummaries.size} many variants)"
        )
        // let's reconcile.
        val cs     = callSummaries.head
        val min    = Math.max(0, callSummaries.iterator.map(_.minArg).min)
        val max    = callSummaries.iterator.map(_.maxArg).max
        val numArg = if (min == 0) max + 1 else max
        createMethodStub(
          cs.name,
          cs.fullName,
          cs.signature,
          cs.dispatchType,
          numArg,
          dstGraph,
          startWithInst = Some(min == 0)
        )
      }
    }
  }

  override def finish(): Unit = {
    methodToParameterCount.clear()
    super.finish()
  }

}

object MethodStubCreator {

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

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

  // this method is hopelessly confused about the meaning of parameterCount, in the sense of whether INST is counted, and about index and order.
  def createMethodStub(
    name: String,
    fullName: String,
    signature: String,
    dispatchType: String,
    parameterCount: Int,
    dstGraph: DiffGraphBuilder,
    isExternal: Boolean = true,
    astParentType: String = NodeTypes.NAMESPACE_BLOCK,
    astParentFullName: String = "<global>",
    startWithInst: Option[Boolean] = None // None means: figure it out from STATIC_DISPATCH
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

    if (startWithInst.isEmpty) {
      // this all looks wrong / legacy / confused, but don't touch it for now
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
    } else {
      val ran = if (startWithInst.get) Range(0, parameterCount) else Range(1, parameterCount + 1)
      for (parameterOrder <- ran) {
        val nameAndCode = s"p$parameterOrder"
        val param = NewMethodParameterIn()
          .code(nameAndCode)
          .order(parameterOrder)
          .index(parameterOrder)
          .name(nameAndCode)
          .evaluationStrategy(EvaluationStrategies.BY_VALUE)
          .typeFullName("ANY")

        dstGraph.addNode(param)
        dstGraph.addEdge(methodNode, param, EdgeTypes.AST)
      }
    }

    val blockNode = NewBlock()
      .order(1)
      .argumentIndex(1)
      .typeFullName(Defines.Any)

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
