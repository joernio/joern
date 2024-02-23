package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, *}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.{ConcurrentWriterCpgPass, CpgPass}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class StaticCallLinker(cpg: Cpg) extends ConcurrentWriterCpgPass[Seq[Call]](cpg) {

  import StaticCallLinker._

  override def generateParts(): Array[Seq[Call]] = {
    val size = cpg.call.size
    val batchSize =
      if (size > ConcurrentTaskUtil.MAX_POOL_SIZE)
        size / ConcurrentTaskUtil.MAX_POOL_SIZE
      else ConcurrentTaskUtil.MAX_POOL_SIZE
    cpg.call.grouped(batchSize).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, calls: Seq[Call]): Unit = {
    calls.foreach { call =>
      try {
        linkCall(call, builder)
      } catch {
        case exception: Exception =>
          throw new RuntimeException(exception)
      }
    }
  }

  private def linkCall(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    call.dispatchType match {
      case DispatchTypes.STATIC_DISPATCH | DispatchTypes.INLINED =>
        linkStaticCall(call, dstGraph)
      case DispatchTypes.DYNAMIC_DISPATCH =>
      // Do nothing
      case _ => logger.warn(s"Unknown dispatch type on dynamic CALL ${call.code}")
    }
  }

  private def linkStaticCall(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    val resolvedMethodOption = cpg.method.fullNameExact(call.methodFullName)
    resolvedMethodOption.foreach(dst => dstGraph.addEdge(call, dst, EdgeTypes.CALL))
  }

}

object StaticCallLinker {
  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])
}
