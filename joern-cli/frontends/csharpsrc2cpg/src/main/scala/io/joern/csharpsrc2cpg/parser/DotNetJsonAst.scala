package io.joern.csharpsrc2cpg.parser

import org.slf4j.LoggerFactory

object DotNetJsonAst {

  private val logger                     = LoggerFactory.getLogger(getClass)
  private val QualifiedClassName: String = DotNetJsonAst.getClass.getName

  def fromString(nodeName: String, fileName: String): DotNetParserNode = {
    try {
      val clazz = Class.forName(s"$QualifiedClassName${nodeName.stripPrefix("ast.")}$$")
      clazz.getField("MODULE$").get(clazz).asInstanceOf[DotNetParserNode]
    } catch {
      case _: Throwable =>
        logger.warn(s"`$nodeName` AST type is not handled. We found this inside '$fileName'")
        NotHandledType
    }
  }

  sealed trait DotNetParserNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  sealed trait BaseExpr extends DotNetParserNode

  object NotHandledType extends DotNetParserNode

  object CompilationUnit extends BaseExpr

  object NamespaceDeclaration extends BaseExpr

  sealed trait DeclarationExpr extends BaseExpr

  object ClassDeclaration extends DeclarationExpr

  object UsingDirective extends DeclarationExpr

  sealed trait IdentifierNode extends BaseExpr

  object IdentifierName extends IdentifierNode

  object QualifiedName extends IdentifierNode

}

object ParserKeys {

  val FileName    = "FileName"
  val AstRoot     = "AstRoot"
  val MetaData    = "MetaData"
  val Kind        = "Kind"
  val LineStart   = "LineStart"
  val LineEnd     = "LineEnd"
  val ColumnStart = "ColumnStart"
  val ColumnEnd   = "ColumnEnd"
  val Usings      = "Usings"
  val Members     = "Members"
  val Name        = "Name"
  val Value       = "Value"
  val Identifier  = "Identifier"
  val Right       = "Right"
  val Left        = "Left"
}
