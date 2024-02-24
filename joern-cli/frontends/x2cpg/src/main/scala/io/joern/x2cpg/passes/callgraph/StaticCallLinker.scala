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

  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])
  val BATCH_SIZE             = 100

  override def generateParts(): Array[Seq[Call]] = {
    val size = cpg.call.size
    cpg.call.grouped(BATCH_SIZE).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, calls: Seq[Call]): Unit = {
    calls.foreach { call =>
      try {
        call.dispatchType match {
          case DispatchTypes.STATIC_DISPATCH | DispatchTypes.INLINED =>
            val resolvedMethodOption = cpg.method.fullNameExact(call.methodFullName)
            resolvedMethodOption.foreach(dst => builder.addEdge(call, dst, EdgeTypes.CALL))
            logger.debug(s"Total ${resolvedMethodOption.size} METHOD nodes found for -> ${call.methodFullName}")
          case DispatchTypes.DYNAMIC_DISPATCH =>
          // Do nothing
          case _ => logger.warn(s"Unknown dispatch type on dynamic CALL ${call.code}")
        }
      } catch {
        case exception: Exception =>
          throw new RuntimeException(exception)
      }
    }
  }
}
