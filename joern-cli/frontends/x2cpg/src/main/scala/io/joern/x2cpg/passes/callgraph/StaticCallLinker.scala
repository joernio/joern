package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class StaticCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  import StaticCallLinker._
  private val methodFullNameToNode = mutable.Map.empty[String, StoredNode]

  /** Main method of enhancement - to be implemented by child class
    */
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    cpg.method.foreach { method =>
      methodFullNameToNode.put(method.fullName, method)
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
      dstGraph.addEdge(call, resolvedMethodOption.get, EdgeTypes.CALL)
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
