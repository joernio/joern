package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.google.common.cache.CacheBuilder
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional
import io.joern.x2cpg.Defines.UnresolvedNamespace
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants

class TypeInferencePass(cpg: Cpg) extends ForkJoinParallelCpgPass[Call](cpg) {

  private val cache = new GuavaCache(CacheBuilder.newBuilder().build[String, Option[Method]]())
  private val resolvedMethodIndex = cpg.method
    .filterNot(_.fullName.startsWith(Defines.UnresolvedNamespace))
    .filterNot(_.signature.startsWith(Defines.UnresolvedSignature))
    .groupBy(_.name)

  private case class NameParts(typeDecl: Option[String], signature: String)

  override def generateParts(): Array[Call] = {
    cpg.call
      .filter(_.signature.startsWith(Defines.UnresolvedSignature))
      .filterNot { _.name.startsWith(UnresolvedNamespace) }
      .toArray
  }

  private def isMatchingMethod(method: Method, call: Call, callNameParts: NameParts): Boolean = {
    // An erroneous `this` argument is added for unresolved calls to static methods.
    val argSizeMod           = if (method.modifier.modifierType.iterator.contains(ModifierTypes.STATIC)) 1 else 0
    lazy val methodNameParts = getNameParts(method.name, method.fullName)

    val parameterSizesMatch =
      (method.parameter.size == (call.argument.size - argSizeMod))

    lazy val argTypesMatch = doArgumentTypesMatch(method: Method, call: Call, skipCallThis = argSizeMod == 1)

    lazy val typeDeclMatches = (callNameParts.typeDecl == methodNameParts.typeDecl)

    parameterSizesMatch && argTypesMatch && typeDeclMatches
  }

  /** Check if argument types match by comparing exact full names. An argument type of `ANY` always matches.
    *
    * TODO: Take inheritance hierarchies into account
    */
  private def doArgumentTypesMatch(method: Method, call: Call, skipCallThis: Boolean): Boolean = {
    val callArgs = if (skipCallThis) call.argument.toList.tail else call.argument.toList

    val hasDifferingArg = method.parameter.zip(callArgs).exists { case (parameter, argument) =>
      val maybeArgumentType = Option(argument.property(PropertyNames.TypeFullName))
        .map(_.toString())
        .getOrElse(TypeConstants.Any)

      val argMatches = maybeArgumentType == TypeConstants.Any || maybeArgumentType == parameter.typeFullName

      !argMatches
    }

    !hasDifferingArg
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
    val argTypes =
      call.argument.flatMap(arg => Option(arg.property(PropertyNames.TypeFullName)).map(_.toString)).mkString(":")
    val callKey =
      s"${call.methodFullName}:$argTypes"
    cache.get(callKey).toScala.getOrElse {
      val callNameParts = getNameParts(call.name, call.methodFullName)
      resolvedMethodIndex.get(call.name).flatMap { candidateMethods =>
        val candidateMethodsIter = candidateMethods.iterator
        val uniqueMatchingMethod =
          candidateMethodsIter.find(isMatchingMethod(_, call, callNameParts)).flatMap { method =>
            val otherMatchingMethod = candidateMethodsIter.find(isMatchingMethod(_, call, callNameParts))
            // Only return a resulting method if exactly one matching method is found.
            Option.when(otherMatchingMethod.isEmpty)(method)
          }
        cache.put(callKey, uniqueMatchingMethod)
        uniqueMatchingMethod
      }
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    getReplacementMethod(call).foreach { replacementMethod =>
      diffGraph.setNodeProperty(call, PropertyNames.MethodFullName, replacementMethod.fullName)
      diffGraph.setNodeProperty(call, PropertyNames.Signature, replacementMethod.signature)
      diffGraph.setNodeProperty(call, PropertyNames.TypeFullName, replacementMethod.methodReturn.typeFullName)
    }
  }
}
