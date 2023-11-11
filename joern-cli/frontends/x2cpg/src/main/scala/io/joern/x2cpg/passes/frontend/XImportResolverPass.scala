package io.joern.x2cpg.passes.frontend

import better.files.File
import io.joern.x2cpg.passes.frontend.ImportsPass.EvaluatedImport
import io.joern.x2cpg.passes.frontend.ImportsPass.EvaluatedImport.*
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import, Tag}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import java.io.File as JFile
import java.nio.charset.StandardCharsets
import java.util.Base64

abstract class XImportResolverPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Import](cpg) {

  protected val logger: Logger = LoggerFactory.getLogger(this.getClass)
  protected val codeRootDir: String = File(
    cpg.metaData.root.headOption.getOrElse(JFile.separator).stripSuffix(JFile.separator)
  ) match
    case f if f.isDirectory => f.pathAsString
    case f                  => f.parent.pathAsString

  override def generateParts(): Array[Import] = cpg.imports.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: Import): Unit = for {
    call <- part.call
    fileName = call.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRootDir)
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

  protected def evaluatedImportToTag(x: EvaluatedImport, importCall: Call, diffGraph: DiffGraphBuilder): Unit =
    importCall.start.newTagNodePair(x.label, x.serialize).store()(diffGraph)

}

object ImportsPass {

  private val sep = ","

  /** An import that has been evaluated as either resolved or not.
    */
  sealed trait EvaluatedImport {
    def label: String

    def serialize: String
  }

  /** An import that has been resolved to a node in the CPG.
    */
  sealed trait ResolvedImport extends EvaluatedImport

  /** An import that has not been successfully resolved to a node in the CPG. This is likely an external dependency.
    */
  sealed trait UnresolvedImport extends EvaluatedImport

  implicit class TagToResolvedImportExt(traversal: Iterator[Tag]) {
    def toEvaluatedImport: Iterator[EvaluatedImport] =
      traversal.flatMap(EvaluatedImport.tagToEvaluatedImport)
  }

  object EvaluatedImport {

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

    def tagToEvaluatedImport(tag: Tag): Option[EvaluatedImport] = Option(tag.name match {
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
      x.split(sep).grouped(2).map(xs => xs(0) -> xs(1).decode).toMap

  }

  implicit class Base64StringExt(str: String) {

    def encode: String = Base64.getEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8))

    def decode: String = new String(Base64.getDecoder.decode(str.getBytes), StandardCharsets.UTF_8)

  }

  case class ResolvedMethod(
    fullName: String,
    alias: String,
    receiver: Option[String] = None,
    override val label: String = RESOLVED_METHOD
  ) extends ResolvedImport {
    override def serialize: String =
      (Seq(OPT_FULL_NAME, fullName.encode, OPT_ALIAS, alias.encode) ++ receiver
        .map(r => Seq(OPT_RECEIVER, r.encode))
        .getOrElse(Seq.empty))
        .mkString(sep)
  }

  case class ResolvedTypeDecl(fullName: String, override val label: String = RESOLVED_TYPE_DECL)
      extends ResolvedImport {
    override def serialize: String = fullName
  }

  case class ResolvedMember(basePath: String, memberName: String, override val label: String = RESOLVED_MEMBER)
      extends ResolvedImport {
    override def serialize: String = Seq(OPT_BASE_PATH, basePath.encode, OPT_NAME, memberName.encode).mkString(sep)
  }

  case class UnknownMethod(
    fullName: String,
    alias: String,
    receiver: Option[String] = None,
    override val label: String = UNKNOWN_METHOD
  ) extends UnresolvedImport {
    override def serialize: String =
      (Seq(OPT_FULL_NAME, fullName.encode, OPT_ALIAS, alias.encode) ++ receiver
        .map(r => Seq(OPT_RECEIVER, r.encode))
        .getOrElse(Seq.empty))
        .mkString(sep)
  }

  case class UnknownTypeDecl(fullName: String, override val label: String = UNKNOWN_TYPE_DECL)
      extends UnresolvedImport {
    override def serialize: String = fullName
  }

  case class UnknownImport(path: String, override val label: String = UNKNOWN_IMPORT) extends UnresolvedImport {
    override def serialize: String = path
  }
}
