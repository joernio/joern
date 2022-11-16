package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.google.common.cache.CacheBuilder
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {

  private val logger         = LoggerFactory.getLogger(this.getClass)
  private val cache          = new GuavaCache(CacheBuilder.newBuilder().build[String, Option[Method]]())
  private val namePartsCache = new GuavaCache(CacheBuilder.newBuilder().build[String, NameParts]())

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
    namePartsCache.get(fullName).toScala.getOrElse {
      val nameParts = fullName match {
        case NamePartsPattern(maybeTd, maybeSig) =>
          val typeDecl  = Option(maybeTd).map(_.init)
          val signature = Option(maybeSig)
          NameParts(typeDecl, signature)

        case _ =>
          logger.warn(s"Could not extract name parts from $fullName")
          NameParts(None, None)
      }
      namePartsCache.put(fullName, nameParts)
      nameParts
    }
  }

  private def getReplacementMethod(call: Call): Option[Method] = {
    cache.get(call.methodFullName).toScala.getOrElse {
      val callNameParts    = getNameParts(call.methodFullName)
      val candidateMethods = cpg.method.internal.nameExact(call.name)

      val uniqueMatchingMethod = candidateMethods.find(isMatchingMethod(_, call, callNameParts)).flatMap { method =>
        val otherMatchingMethod = candidateMethods.find(isMatchingMethod(_, call, callNameParts))
        // Only return a resulting method if exactly one matching method is found.
        Option.when(otherMatchingMethod.isEmpty)(method)
      }
      cache.put(call.methodFullName, uniqueMatchingMethod)
      uniqueMatchingMethod
    }
  }
  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    getReplacementMethod(call).foreach { method =>
      diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, method.fullName)
      diffGraph.setNodeProperty(call, PropertyNames.Signature, method.signature)
      diffGraph.setNodeProperty(call, PropertyNames.TypeFullName, method.methodReturn.typeFullName)
    }
  }
}
