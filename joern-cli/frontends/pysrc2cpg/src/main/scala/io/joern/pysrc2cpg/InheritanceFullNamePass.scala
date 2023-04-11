package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, TypeDecl}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import java.util.regex.Pattern

/** Using some basic heuristics, will try to resolve type full names from types found within the CPG. Requires
  * ImportPass as a pre-requisite.
  */
class InheritanceFullNamePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit =
    cpg.typeDecl
      .filterNot(t =>
        t.inheritsFromTypeFullName == Seq("ANY") || t.inheritsFromTypeFullName == Seq(
          "object"
        ) || t.inheritsFromTypeFullName.isEmpty
      )
      .foreach { t =>
        val resolvedTypeDecls = resolveInheritedTypeFullName(t)
        builder.setNodeProperty(t, PropertyNames.INHERITS_FROM_TYPE_FULL_NAME, resolvedTypeDecls.map(_.fullName))
        cpg.typ
          .fullNameExact(resolvedTypeDecls.fullName.toSeq: _*)
          .foreach(tgt => builder.addEdge(t, tgt, EdgeTypes.INHERITS_FROM))
      }

  private def resolveInheritedTypeFullName(td: TypeDecl): Seq[TypeDecl] = {
    val qualifiedNamesInScope = td.file.ast
      .flatMap {
        case x: Call if x.isCallForImportOut.nonEmpty                          => x.isCallForImportOut.importedEntity
        case x: TypeDecl if !x.fullName.matches(s".*${td.name}([\\w\\._<>])*") => Option(x.fullName)
        case _                                                                 => None
      }
      .filterNot(_.endsWith("<module>"))
      .l
    val matchersInScope = qualifiedNamesInScope.map {
      case x if x.contains(".") =>
        val splitName = x.split("\\.")
        s".*${Pattern.quote(splitName.head)}.*${Pattern.quote(splitName.last)}"
      case x => s".*${Pattern.quote(x)}"
    }.distinct
    val validTypeDecls = cpg.typeDecl.fullName(matchersInScope: _*).l
    validTypeDecls.filter(vt => td.inheritsFromTypeFullName.contains(vt.name))
  }

}
