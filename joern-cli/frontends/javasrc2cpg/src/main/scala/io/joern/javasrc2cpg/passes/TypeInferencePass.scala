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
import io.joern.x2cpg.Defines.UnresolvedNamespace

class TypeInferencePass(cpg: Cpg) extends ConcurrentWriterCpgPass[(String, List[Call])](cpg) {

  private val cache = new GuavaCache(CacheBuilder.newBuilder().build[String, Option[Method]]())
  private val resolvedMethodIndex = cpg.method.internal
    .filterNot(_.fullName.startsWith(Defines.UnresolvedNamespace))
    .filterNot(_.signature.startsWith(Defines.UnresolvedSignature))
    .groupBy(_.name)

  private case class NameParts(typeDecl: Option[String], signature: String)

  override def generateParts(): Array[(String, List[Call])] = {
    cpg.call
      .filter(_.signature.startsWith(Defines.UnresolvedSignature))
      .groupBy(_.methodFullName)
      .filterNot { case (name, _) => name.startsWith(UnresolvedNamespace) }
      .toArray
  }

  private def isMatchingMethod(method: Method, call: Call, callNameParts: NameParts): Boolean = {
    // An erroneous `this` argument is added for unresolved calls to static methods.
    val argSizeMod           = if (method.modifier.modifierType.iterator.contains(ModifierTypes.STATIC)) 1 else 0
    lazy val methodNameParts = getNameParts(method.name, method.fullName)

    (method.parameter.size == (call.argument.size - argSizeMod)) && (callNameParts.typeDecl == methodNameParts.typeDecl)
  }

  private def getNameParts(name: String, fullName: String): NameParts = {
    val Array(qualifiedName, signature) = fullName.split(":", 2)

    val typeDeclName = qualifiedName.stripSuffix(name) match {
      case "" => None

      case typeDeclName => Some(typeDeclName)
    }

    NameParts(typeDeclName, signature)
  }

  private def getReplacementMethod(call: Call): Option[Method] = {
    cache.get(call.methodFullName).toScala.getOrElse {
      val callNameParts = getNameParts(call.name, call.methodFullName)
      resolvedMethodIndex.get(call.name).flatMap { candidateMethods =>
        val uniqueMatchingMethod = candidateMethods.find(isMatchingMethod(_, call, callNameParts)).flatMap { method =>
          val otherMatchingMethod = candidateMethods.find(isMatchingMethod(_, call, callNameParts))
          // Only return a resulting method if exactly one matching method is found.
          Option.when(otherMatchingMethod.isEmpty)(method)
        }
        cache.put(call.methodFullName, uniqueMatchingMethod)
        uniqueMatchingMethod
      }
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, part: (String, List[Call])): Unit = {
    part match {
      case (_, Nil)      => // This should never happen
      case (name, calls) =>
        // Since call full names match, the replacement method for each will be the same
        getReplacementMethod(calls.head) foreach { replacementMethod =>
          calls.foreach { call =>
            diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, replacementMethod.fullName)
            diffGraph.setNodeProperty(call, PropertyNames.Signature, replacementMethod.signature)
            diffGraph.setNodeProperty(call, PropertyNames.TypeFullName, replacementMethod.methodReturn.typeFullName)
          }
        }
    }
  }
}
