package io.joern.javasrc2cpg.passes

import io.joern.javasrc2cpg.util.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, composeMethodLikeSignature, composeUnresolvedSignature}
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call.{PropertyDefaults, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private case class NameParts(typeDecl: Option[String], signature: Option[String])
  private val NamePartsPattern = raw"(.*\.)*.*:(.+\(.*\))".r

  override def generateParts(): Array[Call] = {
    cpg.call.methodFullName(s".*${TypeConstants.UnresolvedType}.*").toArray
  }

  private def isMatchingMethod(method: Method, call: Call): Boolean = {
    method.parameter.size == call.argument.size
  }

  private def getNameParts(fullName: String): NameParts = {
    fullName match {
      case NamePartsPattern(maybeTd, maybeSig) =>
        val typeDecl = Option(maybeTd).map(_.init)
        val signature = Option(maybeSig)
        NameParts(typeDecl, signature)

      case _ =>
        logger.warn(s"Could not extract name parts from $fullName")
        NameParts(None, None)
    }
  }

  private def updateUnresolvedName(diffGraphBuilder: DiffGraphBuilder, call: Call): Unit = {
    val nameParts = getNameParts(call.methodFullName)
    // TODO There is currently still an argument edge to the receiver. Adjust this
    //  when that is removed.
    val paramCount = call.argument.size - call.receiver.size

    val newTypeDecl =
      nameParts
        .typeDecl
        .map(_.replaceAll(TypeConstants.UnresolvedType, Defines.UnresolvedNamespace))
        .getOrElse(Defines.UnresolvedNamespace)

    val newSignature = nameParts.signature match {
      case Some(signature) if signature.contains(TypeConstants.UnresolvedType) =>
        composeUnresolvedSignature(paramCount)

      case Some(signature) => signature

      case None => composeUnresolvedSignature(paramCount)
    }

    val newMethodFullName = composeMethodFullName(newTypeDecl, call.name, newSignature)

    diffGraphBuilder.setNodeProperty(call, PropertyNames.MethodFullName, newMethodFullName)
    diffGraphBuilder.setNodeProperty(call, PropertyNames.Signature, newSignature)

    if (call.typeFullName.contains(TypeConstants.UnresolvedType)) {
      diffGraphBuilder.setNodeProperty(call, PropertyNames.TypeFullName, PropertyDefaults.TypeFullName)
    }
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
          updateUnresolvedName(diffGraph, call)
        }

      case None =>
      // Call is to and unresolved external method, so we can't get any more information here.
      updateUnresolvedName(diffGraph, call)
    }
  }
}
