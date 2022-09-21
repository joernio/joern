package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Call] = {
    cpg.call.methodFullName(s".*(${Defines.UnresolvedNamespace}|${Defines.UnresolvedSignature}).*").toArray
  }

  private def isMatchingMethod(method: Method, call: Call): Boolean = {
    val receiverOffset = call.receiver.size
    method.parameter.size == (call.argument.size - receiverOffset)
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    val candidateMethods = cpg.method.internal.nameExact(call.name)

    candidateMethods.find(isMatchingMethod(_, call)) match {
      case Some(method) =>
        val otherMatchingMethod = candidateMethods.find(isMatchingMethod(_, call))
        if (otherMatchingMethod.isEmpty) {
          diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, method.fullName)
          diffGraph.setNodeProperty(call, PropertyNames.Signature, method.signature)
          diffGraph.setNodeProperty(call, PropertyNames.TypeFullName, method.methodReturn.typeFullName)
        } else {
          logger.info(s"Found multiple matching internal methods for unresolved call ${call.methodFullName}")
        }

      case None => // Call is to and unresolved external method, so we can't get any more information here.
    }
  }
}
