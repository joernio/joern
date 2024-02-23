package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.{ConcurrentWriterCpgPass, CpgPass}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class StaticCallLinker(cpg: Cpg) extends ConcurrentWriterCpgPass[Seq[Call]](cpg) {

  import StaticCallLinker.*

  override def generateParts(): Array[Seq[Call]] = {
    val size = cpg.call.size
    val batchSize =
      if (size > ConcurrentTaskUtil.MAX_POOL_SIZE)
        size / ConcurrentTaskUtil.MAX_POOL_SIZE
      else ConcurrentTaskUtil.MAX_POOL_SIZE
    val parts = cpg.call.grouped(batchSize).toArray
    println(s"----> Static call linker gnerated parts -> ${parts.size}")
    parts
  }
  case class TempData(call: Call, dst: Method)
  override def runOnPart(builder: DiffGraphBuilder, calls: Seq[Call]): Unit = {
    val listCalls = new ListBuffer[TempData]
    calls.foreach { call =>
      try {
        linkCall(call, builder, listCalls)
      } catch {
        case exception: Exception =>
          throw new RuntimeException(exception)
      }
    }
    println(s"-------> Static call linker total calls ${listCalls.size}")
  }

  private def linkCall(call: Call, dstGraph: DiffGraphBuilder, listCalls: ListBuffer[TempData]): Unit = {
    call.dispatchType match {
      case DispatchTypes.STATIC_DISPATCH | DispatchTypes.INLINED =>
        linkStaticCall(call, dstGraph, listCalls)
      case DispatchTypes.DYNAMIC_DISPATCH =>
      // Do nothing
      case _ => logger.warn(s"Unknown dispatch type on dynamic CALL ${call.code}")
    }
  }

  private def linkStaticCall(call: Call, dstGraph: DiffGraphBuilder, listCalls: ListBuffer[TempData]): Unit = {
    val resolvedMethodOption = cpg.method.fullNameExact(call.methodFullName)
    resolvedMethodOption.foreach(dst => listCalls += TempData(call, dst))
  }

}

object StaticCallLinker {
  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])
}
