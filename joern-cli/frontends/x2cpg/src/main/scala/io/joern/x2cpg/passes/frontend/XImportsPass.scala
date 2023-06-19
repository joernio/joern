package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method, Tag, TypeDecl}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment

import java.io.{File => JFile}

abstract class XImportsPass(cpg: Cpg) extends ConcurrentWriterCpgPass[(Call, Assignment)](cpg) {

  protected val importCallName: String
  protected val codeRoot: String = cpg.metaData.root.headOption.getOrElse(JFile.separator)

  override def generateParts(): Array[(Call, Assignment)] = cpg
    .call(importCallName)
    .flatMap(importCallToPart)
    .toArray

  protected def importCallToPart(x: Call): Iterator[(Call, Assignment)]

  override def runOnPart(diffGraph: DiffGraphBuilder, part: (Call, Assignment)): Unit = {
    val (call, assignment) = part
    val importedEntity     = importedEntityFromCall(call)
    val importedAs         = assignment.target.code
    createImportNodeAndLink(importedEntity, importedAs, Some(call), diffGraph)
    optionalResolveImport(
      call.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRoot),
      call,
      importedEntity,
      importedAs,
      diffGraph
    )
  }

  protected def importedEntityFromCall(call: Call): String

  protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {}

}

object ImportsPass {

  implicit class ResolvedNodeExt(val traversal: Seq[String]) {
    def toResolvedImport(cpg: Cpg): Seq[ResolvedImport] = {
      val resolvedEntities =
        traversal.flatMap(x => cpg.typeDecl.fullNameExact(x) ++ cpg.method.fullNameExact(x)).collect {
          case x: Method   => ResolvedMethod(x.fullName)
          case x: TypeDecl => ResolvedTypeDecl(x.fullName)
        }
      if (resolvedEntities.isEmpty) {
        traversal.filterNot(_.contains("__init__.py")).map(x => UnknownImport(x))
      } else {
        resolvedEntities
      }
    }
  }

  sealed trait ResolvedImport {
    def label: String
  }

  object ResolvedImport {

    val RESOLVED_METHOD    = "RESOLVED_METHOD"
    val RESOLVED_TYPE_DECL = "RESOLVED_TYPE_DECL"
    val RESOLVED_MEMBER    = "RESOLVED_MEMBER"
    val UNKNOWN_METHOD     = "UNKNOWN_METHOD"
    val UNKNOWN_TYPE_DECL  = "UNKNOWN_TYPE_DECL"
    val UNKNOWN_IMPORT     = "UNKNOWN_IMPORT"

    def tagToResolvedImport(tag: Tag): Option[ResolvedImport] = Option(tag.name match {
      case RESOLVED_METHOD    => ResolvedMethod(tag.value)
      case RESOLVED_TYPE_DECL => ResolvedTypeDecl(tag.value)
      case RESOLVED_MEMBER =>
        val splitTag       = tag.value.split('.')
        val (base, member) = ((splitTag diff Seq(splitTag.last)).mkString("."), splitTag.last)
        ResolvedMember(base, member)
      case UNKNOWN_METHOD    => UnknownMethod(tag.value)
      case UNKNOWN_TYPE_DECL => UnknownTypeDecl(tag.value)
      case UNKNOWN_IMPORT    => UnknownImport(tag.value)
      case _                 => null
    })
  }

  case class ResolvedMethod(fullName: String, override val label: String = "RESOLVED_METHOD") extends ResolvedImport

  case class ResolvedTypeDecl(fullName: String, override val label: String = "RESOLVED_TYPE_DECL")
      extends ResolvedImport

  case class ResolvedMember(basePath: String, memberName: String, override val label: String = "RESOLVED_MEMBER")
      extends ResolvedImport

  case class UnknownMethod(fullName: String, override val label: String = "UNKNOWN_METHOD") extends ResolvedImport

  case class UnknownTypeDecl(fullName: String, override val label: String = "UNKNOWN_TYPE_DECL") extends ResolvedImport

  case class UnknownImport(path: String, override val label: String = "UNKNOWN_IMPORT") extends ResolvedImport
}
