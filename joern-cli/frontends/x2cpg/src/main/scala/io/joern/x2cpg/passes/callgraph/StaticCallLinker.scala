package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class StaticCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  import StaticCallLinker._
  private val methodFullNameToNode = mutable.Map.empty[String, List[Method]]

  override def run(dstGraph: DiffGraphBuilder): Unit = {

    cpg.method.foreach { method =>
      methodFullNameToNode.updateWith(method.fullName) {
        case Some(l) => Some(method :: l)
        case None    => Some(List(method))
      }
    }

    cpg.call.foreach { call =>
      try {
        linkCall(call, dstGraph)
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
    val resolvedMethodOption = methodFullNameToNode.get(call.methodFullName)
    if (resolvedMethodOption.isDefined) {
      resolvedMethodOption.get.foreach { dst =>
        dstGraph.addEdge(call, dst, EdgeTypes.CALL)
      }
    } else {
      logger.info(
        s"Unable to link static CALL with METHOD_FULL_NAME ${call.methodFullName}, NAME ${call.name}, " +
          s"SIGNATURE ${call.signature}, CODE ${call.code}"
      )
    }
  }

}

object StaticCallLinker {
  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])
}
