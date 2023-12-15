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

  object MethodDeclaration extends DeclarationExpr

  object UsingDirective extends BaseExpr

  object Parameter extends BaseExpr

  sealed trait TypeIdentifier extends BaseExpr

  object PredefinedType extends TypeIdentifier

  object Block extends BaseExpr

  sealed trait IdentifierNode extends BaseExpr

  object IdentifierName extends IdentifierNode

  object QualifiedName extends IdentifierNode

}

object ParserKeys {

  val FileName      = "FileName"
  val AstRoot       = "AstRoot"
  val Body          = "Body"
  val MetaData      = "MetaData"
  val Kind          = "Kind"
  val LineStart     = "LineStart"
  val LineEnd       = "LineEnd"
  val ColumnStart   = "ColumnStart"
  val ColumnEnd     = "ColumnEnd"
  val Keyword       = "Keyword"
  val Usings        = "Usings"
  val Members       = "Members"
  val Modifiers     = "Modifiers"
  val Name          = "Name"
  val Parameters    = "Parameters"
  val ParameterList = "ParameterList"
  val Value         = "Value"
  val Identifier    = "Identifier"
  val ReturnType    = "ReturnType"
  val Right         = "Right"
  val Left          = "Left"
}
