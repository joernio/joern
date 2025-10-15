package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.google.common.cache.CacheBuilder
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{Cpg, ModifierTypes, Properties}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.jdk.OptionConverters.RichOptional
import io.joern.x2cpg.Defines.UnresolvedNamespace
import io.shiftleft.codepropertygraph.generated.PropertyNames
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
    if (call.name == "createScript") {
      println(s"\n  Checking method: ${method.fullName}")
    }
    // An erroneous `this` argument is added for unresolved calls to static methods.
    val argSizeMod           = if (method.modifier.modifierType.iterator.contains(ModifierTypes.STATIC)) 1 else 0
    lazy val methodNameParts = getNameParts(method.name, method.fullName)

    val parameterSizesMatch =
      (method.parameter.size == (call.argument.size - argSizeMod))
    if (call.name == "createScript") {
      println(s"    parameterSizesMatch: $parameterSizesMatch (${method.parameter.size} == ${call.argument.size} - $argSizeMod)")
    }
    lazy val argTypesMatch = doArgumentTypesMatch(method: Method, call: Call, skipCallThis = argSizeMod == 1)
    if (call.name == "createScript" && parameterSizesMatch) {
      println(s"    argTypesMatch: $argTypesMatch")
    }
    lazy val typeDeclMatches = (callNameParts.typeDecl == methodNameParts.typeDecl)

    parameterSizesMatch && argTypesMatch && typeDeclMatches
  }

  /** Check if argument types match by comparing exact full names. An argument type of `ANY` always matches.
    *
    * TODO: Take inheritance hierarchies into account
    */
  private def doArgumentTypesMatch(method: Method, call: Call, skipCallThis: Boolean): Boolean = {
    val callArgs = if (skipCallThis) call.argument.toList.tail else call.argument.toList
//    if (call.name == "createScript") {
//      println(s"\n    === doArgumentTypesMatch ===")
//      println(s"    skipCallThis: $skipCallThis")
//      println(s"    call.argument.size: ${call.argument.size}")
//      println(s"    callArgs.size after skip: ${callArgs.size}")
//      println(s"    method.parameter.size: ${method.parameter.size}")
//    }
    val hasDifferingArg = method.parameter.zip(callArgs).zipWithIndex.exists { case ((parameter, argument), idx) =>
      val maybeArgumentType = argument.propertyOption(Properties.TypeFullName).getOrElse(TypeConstants.Any)
      val argMatches        = maybeArgumentType == TypeConstants.Any || maybeArgumentType == parameter.typeFullName || (maybeArgumentType == TypeConstants.NULL && !isPrimitiveType(parameter.typeFullName)) || isSubtypeOf(maybeArgumentType, parameter.typeFullName)
//      if (call.name == "createScript") {
//        println(s"    [$idx] Param: ${parameter.name} (${parameter.typeFullName}) vs Arg: ${argument.code} (${maybeArgumentType})")
//        println(s"        Match: $argMatches")
//      }
      !argMatches
    }
//    if (call.name == "createScript") {
//      println(s"    hasDifferingArg: $hasDifferingArg")
//      println(s"    === End doArgumentTypesMatch ===\n")
//    }
    !hasDifferingArg
  }
  private def isPrimitiveType(typeName: String): Boolean = {
    Set("byte", "short", "int", "long", "float", "double", "boolean", "char").contains(typeName)
  }

  private def isSubtypeOf(argumentType: String, parameterType: String): Boolean = {
    if (argumentType == parameterType) {
      return true
    }

    cpg.typeDecl.fullNameExact(argumentType).headOption match {
      case Some(typeDecl) =>
        typeDecl.inheritsFromTypeFullName.contains(parameterType)
      case None =>
        false
    }
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
    val argTypes = call.argument.property(Properties.TypeFullName).mkString(":")
    val callKey  = s"${call.methodFullName}:$argTypes"
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
