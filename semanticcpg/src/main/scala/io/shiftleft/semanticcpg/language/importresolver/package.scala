package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.Tag

import java.nio.charset.StandardCharsets
import java.util.Base64

package object importresolver {

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

  object EvaluatedImport {

    val sep = ","

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

  import EvaluatedImport.*

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
