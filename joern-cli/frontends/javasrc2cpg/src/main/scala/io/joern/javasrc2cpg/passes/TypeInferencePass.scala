package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private case class NameParts(typeDecl: Option[String], signature: Option[String])

  private val NamePartsPattern = raw"(.*\.)*.*:(.+\(.*\))".r

  override def generateParts(): Array[Call] = {
    cpg.call.methodFullName(s".*(${Defines.UnresolvedNamespace}|${Defines.UnresolvedSignature}).*").toArray
  }

  private def isMatchingMethod(method: Method, call: Call, callNameParts: NameParts): Boolean = {
    // An erroneous `this` argument is added for unresolved calls to static methods.
    val argSizeMod = if (method.modifier.modifierType.iterator.contains(ModifierTypes.STATIC)) 1 else 0
    if (
      method.parameter.size != (call.argument.size - argSizeMod) || callNameParts.typeDecl.contains(
        Defines.UnresolvedNamespace
      )
    ) {
      false
    } else {
      val methodNameParts = getNameParts(method.fullName)
      callNameParts.typeDecl == methodNameParts.typeDecl
    }

  }

  private def getNameParts(fullName: String): NameParts = {
    fullName match {
      case NamePartsPattern(maybeTd, maybeSig) =>
        val typeDecl  = Option(maybeTd).map(_.init)
        val signature = Option(maybeSig)
        NameParts(typeDecl, signature)

      case _ =>
        logger.warn(s"Could not extract name parts from $fullName")
        NameParts(None, None)
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    val callNameParts    = getNameParts(call.methodFullName)
    val candidateMethods = cpg.method.internal.nameExact(call.name)

    candidateMethods.find(isMatchingMethod(_, call, callNameParts)) match {
      case Some(method) =>
        val otherMatchingMethod = candidateMethods.find(isMatchingMethod(_, call, callNameParts))
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
