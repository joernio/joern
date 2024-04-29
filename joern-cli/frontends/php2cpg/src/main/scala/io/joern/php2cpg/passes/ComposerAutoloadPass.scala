package io.joern.php2cpg.passes

import io.joern.x2cpg.{Defines, X2Cpg}
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Method, TypeDecl}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Composer's autoload feature implicitly brings in dependencies into a file. This pass will detect such imports and
  * fully qualify type names referencing non-stub types. This pass also considers types brought in by the dependency
  * downloader.
  */
class ComposerAutoloadPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  private val typeMap   = mutable.Map.empty[String, String]
  private val methodMap = mutable.Map.empty[String, String]

  override def init(): Unit = {
    cpg.typeDecl.filter(x => x.name != x.fullName && x.isExternal).foreach { t =>
      typeMap.put(t.name, t.fullName)
      t.method.foreach { m =>
        methodMap.put(s"${t.name}->${m.name}", m.fullName)
      }
    }
  }

  private def isAutoloaded(method: Method) = method._containsOut
    .collect { case x: Call if x.name == "require" => x }
    .flatMap(_.argument.code)
    .map(X2Cpg.stripQuotes)
    .contains("vendor/autoload.php")

  /** Collects methods within a module.
    */
  private def findMethods(module: Method): Iterator[Method] = {
    Iterator(module) ++ module.containsOut.flatMap {
      case x: TypeDecl => x.method.flatMap(findMethods)
      case x: Method   => Iterator(x) ++ x.containsOut.collectAll[Method].flatMap(findMethods)
      case _           => Iterator.empty
    }
  }

  override def generateParts(): Array[Method] =
    cpg.method.isModule.filter(isAutoloaded).flatMap(findMethods).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Method): Unit = {
    part._containsOut.foreach {
      case call: Call             => fullyQualifyCall(call, builder)
      case identifier: Identifier => fullyQualifyIdentifier(identifier, builder)
      case _                      =>
    }
  }

  private def fullyQualifyCall(call: Call, builder: DiffGraphBuilder): Unit = {
    if (call.typeFullName != Defines.Any) {
      typeMap.get(call.typeFullName).foreach(builder.setNodeProperty(call, PropertyNames.TYPE_FULL_NAME, _))
    }
    methodMap.get(call.methodFullName).foreach(builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, _))
  }

  private def fullyQualifyIdentifier(identifier: Identifier, builder: DiffGraphBuilder): Unit = {
    if (identifier.typeFullName != Defines.Any) {
      typeMap.get(identifier.typeFullName).foreach(builder.setNodeProperty(identifier, PropertyNames.TYPE_FULL_NAME, _))
    }
  }

}
