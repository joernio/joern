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
  sealed trait BaseExprStmt extends ParserNode

  object File       extends ParserNode
  object GenDecl    extends ParserNode
  object ImportSpec extends ParserNode
  object BasicLit   extends ParserNode
  object FuncDecl   extends ParserNode
  object BlockStmt  extends ParserNode
  object DeclStmt   extends ParserNode
  object ValueSpec  extends ParserNode
  object Ident      extends ParserNode
  object AssignStmt extends ParserNode
  object ExprStmt   extends BaseExprStmt
  object BinaryExpr extends BaseExprStmt
  object UnaryExpr  extends BaseExprStmt
  object StarExpr   extends BaseExprStmt

  object IncDecStmt extends ParserNode
}

object ParserKeys {

  val Body          = "Body"
  val Decl          = "Decl"
  val Decls         = "Decls"
  val Kind          = "Kind"
  val List          = "List"
  val Lhs           = "Lhs"
  val Name          = "Name"
  val Names         = "Names"
  val NodeColNo     = "node_col_no"
  val NodeColEndNo  = "node_col_no_end"
  val NodeFileName  = "node_filename"
  val NodeLineNo    = "node_line_no"
  val NodeLineEndNo = "node_line_no_end"
  val NodeType      = "node_type"
  val Op            = "Op"
  val Path          = "Path"
  val Rhs           = "Rhs"
  val Specs         = "Specs"
  val Tok           = "Tok"
  val Type          = "Type"
  val Value         = "Value"
  val Values        = "Values"
  val X             = "X"
  val Y             = "Y"
}
