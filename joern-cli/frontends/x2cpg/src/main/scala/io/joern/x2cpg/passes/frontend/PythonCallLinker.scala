package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

// link python calls to methods only by their name(not full name)
class PythonCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  import PythonCallLinker._

  private val methodNameToNode = mutable.Map.empty[String, List[Method]]

  override def run(dstGraph: DiffGraphBuilder): Unit = {

    cpg.method.foreach { method =>
      methodNameToNode.updateWith(method.name) {
        case Some(l) => Some(method :: l)
        case None    => Some(List(method))
      }
    }

    cpg.call.foreach { call =>
      try {
        linkCallOnlyByShortName(call, dstGraph)
      } catch {
        case exception: Exception =>
          throw new RuntimeException(exception)
      }
    }
  }

  private def linkCallOnlyByShortName(call: Call, dstGraph: DiffGraphBuilder): Unit = {
    val resolvedMethodOption = methodNameToNode.get(call.name)
    if (resolvedMethodOption.isDefined) {
      resolvedMethodOption.get.foreach { dst =>
        dstGraph.addEdge(call, dst, EdgeTypes.CALL)
      }
    } else {
      logger.info(
        s"Unable to link CALL with METHOD_NAME ${call.name}, NAME ${call.name}, " +
          s"SIGNATURE ${call.signature}, CODE ${call.code}"
      )
    }
  }

}
object PythonCallLinker {
  private val logger: Logger = LoggerFactory.getLogger(classOf[PythonCallLinker])
}
