package io.joern.x2cpg.passes.base

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.base.MethodStubCreator.{createMethodStub, logger}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

case class CallSummary(
  name: String,
  fullName: String,
  signature: String,
  dispatchType: String,
  minArg: Int,
  maxArg: Int,
  numArg: Int
)

/** This pass has no other pass as prerequisite.
  */
class MethodStubCreator(cpg: Cpg) extends CpgPass(cpg) {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val methodToParameterCount = mutable.HashMap[String, mutable.HashSet[CallSummary]]()

    for {
      call <- cpg.call
      if call.methodFullName != Defines.DynamicCallUnknownFullName &&
        cpg.method.fullNameExact(call.methodFullName).isEmpty
    } {
      methodToParameterCount
        .getOrElseUpdate(call.methodFullName, mutable.HashSet.empty[CallSummary])
        .add(
          CallSummary(
            call.name,
            call.methodFullName,
            call.signature,
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

}

object MethodStubCreator {

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

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
          .typeFullName(Defines.Any)

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
          .typeFullName(Defines.Any)

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
      .typeFullName(Defines.Any)

    dstGraph.addNode(methodReturn)
    dstGraph.addEdge(methodNode, methodReturn, EdgeTypes.AST)

    methodNode
  }
}
