package io.joern.javasrc2cpg.passes

import com.github.javaparser.symbolsolver.cache.GuavaCache
import com.google.common.cache.CacheBuilder
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{Cpg, ModifierTypes, Properties}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import scala.jdk.OptionConverters.RichOptional
import io.joern.x2cpg.Defines.UnresolvedNamespace
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.{PrimitiveTypes, TypeConstants}

class TypeInferencePass(cpg: Cpg) extends ForkJoinParallelCpgPass[Call](cpg) {

  private val cache               = new GuavaCache(CacheBuilder.newBuilder().build[String, Option[Method]]())
  private val resolvedMethodIndex = cpg.method
    .filterNot(_.fullName.startsWith(Defines.UnresolvedNamespace))
    .filterNot(_.signature.startsWith(Defines.UnresolvedSignature))
    .groupBy(_.name)

  private val directParentTypes: Map[String, Set[String]] =
    cpg.typeDecl.map(typeDecl => typeDecl.fullName -> typeDecl.inheritsFromTypeFullName.toSet).toMap

  private def transitiveAncestors(typeName: String, visited: Set[String] = Set.empty): Set[String] = {
    if (visited.contains(typeName)) Set.empty
    else {
      val parents = directParentTypes.getOrElse(typeName, Set.empty)
      parents ++ parents.flatMap(parent => transitiveAncestors(parent, visited + typeName))
    }
  }

  private val ancestorCache: Map[String, Set[String]] =
    cpg.typeDecl.map { typeDecl =>
      val fromGraph = typeDecl.baseTypeDeclTransitive.fullName.toSet
      typeDecl.fullName -> (fromGraph ++ transitiveAncestors(typeDecl.fullName))
    }.toMap

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

    lazy val argTypesMatch = doArgumentTypesMatch(method, call, skipCallThis = argSizeMod == 1)

    lazy val typeDeclMatches = (callNameParts.typeDecl == methodNameParts.typeDecl)

    parameterSizesMatch && argTypesMatch && typeDeclMatches
  }

  private def isSubtype(argType: String, paramType: String): Boolean = {
    argType == paramType || ancestorCache.getOrElse(argType, Set.empty).contains(paramType)
  }

  private def isAssignableArgumentType(argType: String, paramType: String): Boolean = {
    argType == TypeConstants.Any ||
    argType == paramType ||
    (argType == TypeConstants.Null && !PrimitiveTypes.contains(paramType)) ||
    isSubtype(argType, paramType) ||
    (paramType == TypeConstants.Object && !PrimitiveTypes.contains(argType))
  }

  /** Check if argument types are assignable to method parameter types, including inheritance. An argument type of `ANY`
    * always matches.
    */
  private def doArgumentTypesMatch(method: Method, call: Call, skipCallThis: Boolean): Boolean = {
    val callArgs = if (skipCallThis) call.argument.toList.tail else call.argument.toList

    val hasDifferingArg = method.parameter.zip(callArgs).exists { case (parameter, argument) =>
      val maybeArgumentType = argument.propertyOption(Properties.TypeFullName).getOrElse(TypeConstants.Any)
      !isAssignableArgumentType(maybeArgumentType, parameter.typeFullName)
    }

    !hasDifferingArg
  }

  private def parameterTypes(method: Method): List[String] =
    method.parameter.sortBy(_.index).map(_.typeFullName).toList

  private def isAtLeastAsSpecificAs(moreSpecificCandidate: Method, other: Method): Boolean = {
    parameterTypes(moreSpecificCandidate).zip(parameterTypes(other)).forall { case (specific, general) =>
      isSubtype(specific, general)
    }
  }

  private def isMoreSpecificThan(candidate: Method, other: Method): Boolean =
    isAtLeastAsSpecificAs(candidate, other) && !isAtLeastAsSpecificAs(other, candidate)

  private def uniqueMostSpecificMethod(applicable: List[Method]): Option[Method] = {
    val mostSpecific = applicable.filter { candidate =>
      !applicable.exists(other => other != candidate && isMoreSpecificThan(other, candidate))
    }
    Option.when(mostSpecific.size == 1)(mostSpecific.head)
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
      val callNameParts        = getNameParts(call.name, call.methodFullName)
      val uniqueMatchingMethod = resolvedMethodIndex.get(call.name).flatMap { candidateMethods =>
        val applicable = candidateMethods.filter(isMatchingMethod(_, call, callNameParts)).toList
        uniqueMostSpecificMethod(applicable)
      }
      cache.put(callKey, uniqueMatchingMethod)
      uniqueMatchingMethod
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
