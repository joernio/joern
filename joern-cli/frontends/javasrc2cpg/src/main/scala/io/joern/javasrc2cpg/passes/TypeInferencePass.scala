package io.joern.javasrc2cpg.passes

import io.joern.javasrc2cpg.util.TypeInfoCalculator.TypeConstants
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Call] = {
    cpg.call.methodFullName(s".*${TypeConstants.UnresolvedType}.*").toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    val candidateMethods = cpg.method.internal.nameExact(call.name).l

    candidateMethods match {
      case Nil           => // Call is to an unresolved external method, so we can't get any more information here.
      case method :: Nil =>
        // This is probably the correct method if the number of arguments is correct.
        if (method.parameter.size == call.argument.size) {
          diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, method.fullName)
          diffGraph.setNodeProperty(call, PropertyNames.Signature, method.signature)

        } else {
          logger.info(
            s"Found non-matching internal method for unresolved call: ${method.fullName} - ${call.methodFullName}"
          )
        }

      case methods =>
        methods.filter(_.parameter.size == call.argument.size) match {
          case method :: Nil =>
            diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, method.fullName)
            diffGraph.setNodeProperty(call, PropertyNames.Signature, method.signature)

          case _ =>
            logger.info(s"Found multiple matching internal methods for unresolved call ${call.methodFullName}")
        }
    }
  }
}
