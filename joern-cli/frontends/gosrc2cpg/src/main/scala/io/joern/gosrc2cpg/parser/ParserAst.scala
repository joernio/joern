package io.joern.gosrc2cpg.parser

object ParserAst {

  private val QualifiedClassName: String = ParserAst.getClass.getName

  def fromString(nodeName: String): ParserNode = {
    val clazz = Class.forName(s"$QualifiedClassName${nodeName.stripPrefix("ast.")}$$")
    clazz.getField("MODULE$").get(clazz).asInstanceOf[ParserNode]
  }

  sealed trait ParserNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  object GenDecl    extends ParserNode
  object ImportSpec extends ParserNode
  object BasicLit   extends ParserNode
  object FuncDecl   extends ParserNode
  object BlockStmt  extends ParserNode
  object DeclStmt   extends ParserNode
  object ValueSpec  extends ParserNode
  object Ident      extends ParserNode
  object AssignStmt extends ParserNode
  object ExprStmt   extends ParserNode
}

object ParserKeys {

  val Path         = "Path"
  val Value        = "Value"
  val Values       = "Values"
  val Name         = "Name"
  val Names        = "Names"
  val Specs        = "Specs"
  val Decls        = "Decls"
  val Decl         = "Decl"
  val NodeFileName = "node_filename"
  val NodeType     = "node_type"
  val NodeLineNo   = "node_line_no"
  val NodeColNo    = "node_col_no"
  val Type         = "Type"
  val List         = "List"
  val Body         = "Body"
}
