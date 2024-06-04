package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class StaticCallLinker(cpg: Cpg) extends ForkJoinParallelCpgPass[Seq[Call]](cpg) with LinkingUtil {

  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])

  override def generateParts(): Array[Seq[Call]] = {
    cpg.call.toList.grouped(MAX_BATCH_SIZE).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, calls: Seq[Call]): Unit = {
    calls.foreach { call =>
      try {
        call.dispatchType match {
          case DispatchTypes.STATIC_DISPATCH | DispatchTypes.INLINED =>
            val resolvedMethods = cpg.method.fullNameExact(call.methodFullName).l
            resolvedMethods.foreach(dst => builder.addEdge(call, dst, EdgeTypes.CALL))
            val size = resolvedMethods.size
            // Add the debug logs with number of METHOD nodes found for given method full name
            if size > 1 then logger.debug(s"Total $size METHOD nodes found for -> ${call.methodFullName}")
          case DispatchTypes.DYNAMIC_DISPATCH =>
          // Do nothing
          case _ => logger.warn(s"Unknown dispatch type on dynamic CALL ${call.code}")
        }
      } catch {
        case exception: Exception =>
          logger.error(s"Exception in StaticCallLinker: ", exception)
      }
    }
  }
}
