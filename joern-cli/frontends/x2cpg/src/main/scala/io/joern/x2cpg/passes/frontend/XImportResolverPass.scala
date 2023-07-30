package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.passes.frontend.ImportsPass.ResolvedImport
import io.joern.x2cpg.passes.frontend.ImportsPass.ResolvedImport._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, Tag}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import java.io.{File => JFile}

abstract class XImportResolverPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Import](cpg) {

  protected val logger: Logger   = LoggerFactory.getLogger(this.getClass)
  protected val codeRoot: String = cpg.metaData.root.headOption.getOrElse(JFile.separator)

  override def generateParts(): Array[Import] = cpg.imports.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Import): Unit = for {
    call <- part.call
    fileName = call.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRoot)
    importedAs     <- part.importedAs
    importedEntity <- part.importedEntity
  } {
    optionalResolveImport(fileName, call, importedEntity, importedAs, builder)
  }

  protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit

  protected def resolvedImportToTag(x: ResolvedImport, importCall: Call, diffGraph: DiffGraphBuilder): Unit =
    importCall.start.newTagNodePair(x.label, x.serialize).store()(diffGraph)

}

object ImportsPass {

  sealed trait ResolvedImport {
    def label: String

    def serialize: String
  }

  implicit class TagToResolvedImportExt(traversal: Traversal[Tag]) {
    def toResolvedImport: Traversal[ResolvedImport] =
      traversal.flatMap(ResolvedImport.tagToResolvedImport)
  }

  object ResolvedImport {

    val RESOLVED_METHOD    = "RESOLVED_METHOD"
    val RESOLVED_TYPE_DECL = "RESOLVED_TYPE_DECL"
    val RESOLVED_MEMBER    = "RESOLVED_MEMBER"
    val UNKNOWN_METHOD     = "UNKNOWN_METHOD"
    val UNKNOWN_TYPE_DECL  = "UNKNOWN_TYPE_DECL"
    val UNKNOWN_IMPORT     = "UNKNOWN_IMPORT"

    val OPT_FULL_NAME = "FULL_NAME"
    val OPT_ALIAS     = "ALIAS"
    val OPT_RECEIVER  = "RECEIVER"
    val OPT_BASE_PATH = "BASE_PATH"
    val OPT_NAME      = "NAME"

    def tagToResolvedImport(tag: Tag): Option[ResolvedImport] = Option(tag.name match {
      case RESOLVED_METHOD =>
        val opts = valueToOptions(tag.value)
        ResolvedMethod(opts(OPT_FULL_NAME), opts(OPT_ALIAS), opts.get(OPT_RECEIVER))
      case RESOLVED_TYPE_DECL => ResolvedTypeDecl(tag.value)
      case RESOLVED_MEMBER =>
        val opts = valueToOptions(tag.value)
        ResolvedMember(opts(OPT_BASE_PATH), opts(OPT_NAME))
      case UNKNOWN_METHOD =>
        val opts = valueToOptions(tag.value)
        UnknownMethod(opts(OPT_FULL_NAME), opts(OPT_ALIAS), opts.get(OPT_RECEIVER))
      case UNKNOWN_TYPE_DECL => UnknownTypeDecl(tag.value)
      case UNKNOWN_IMPORT    => UnknownImport(tag.value)
      case _                 => null
    })

    private def valueToOptions(x: String): Map[String, String] =
      x.split(',').grouped(2).map(xs => xs(0) -> xs(1)).toMap
  }

  case class ResolvedMethod(
    fullName: String,
    alias: String,
    receiver: Option[String] = None,
    override val label: String = RESOLVED_METHOD
  ) extends ResolvedImport {
    override def serialize: String =
      s"$OPT_FULL_NAME,$fullName,$OPT_ALIAS,$alias" + receiver.map(r => s",$OPT_RECEIVER,$r").getOrElse("")
  }

  case class ResolvedTypeDecl(fullName: String, override val label: String = RESOLVED_TYPE_DECL)
      extends ResolvedImport {
    override def serialize: String = fullName
  }

  case class ResolvedMember(basePath: String, memberName: String, override val label: String = RESOLVED_MEMBER)
      extends ResolvedImport {
    override def serialize: String = s"$OPT_BASE_PATH,$basePath,$OPT_NAME,$memberName"
  }

  case class UnknownMethod(
    fullName: String,
    alias: String,
    receiver: Option[String] = None,
    override val label: String = UNKNOWN_METHOD
  ) extends ResolvedImport {
    override def serialize: String =
      s"$OPT_FULL_NAME,$fullName,$OPT_ALIAS,$alias" + receiver.map(r => s",$OPT_RECEIVER,$r").getOrElse("")
  }

  case class UnknownTypeDecl(fullName: String, override val label: String = UNKNOWN_TYPE_DECL) extends ResolvedImport {
    override def serialize: String = fullName
  }

  case class UnknownImport(path: String, override val label: String = UNKNOWN_IMPORT) extends ResolvedImport {
    override def serialize: String = path
  }
}
