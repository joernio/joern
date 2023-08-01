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
  sealed trait BaseExpr extends ParserNode

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
  object ExprStmt   extends ParserNode
  object BinaryExpr extends BaseExpr
  object UnaryExpr  extends BaseExpr
  object StarExpr   extends BaseExpr

  object IncDecStmt     extends ParserNode
  object IfStmt         extends ParserNode
  object ParenExpr      extends BaseExpr
  object SwitchStmt     extends ParserNode
  object CaseClause     extends ParserNode
  object TypeSwitchStmt extends ParserNode
  object TypeAssertExpr extends BaseExpr
  object InterfaceType  extends ParserNode
  object ReturnStmt     extends ParserNode
  object FuncType       extends ParserNode
  object Ellipsis       extends ParserNode
  object SelectorExpr   extends ParserNode
  object ForStmt        extends ParserNode
  object CallExpr       extends BaseExpr
}

object ParserKeys {

  val Assign        = "Assign"
  val Body          = "Body"
  val Cond          = "Cond"
  val Decl          = "Decl"
  val Decls         = "Decls"
  val Else          = "Else"
  val Init          = "Init"
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
  val Post          = "Post"
  val Rhs           = "Rhs"
  val Specs         = "Specs"
  val Tag           = "Tag"
  val Tok           = "Tok"
  val Type          = "Type"
  val Value         = "Value"
  val Values        = "Values"
  val X             = "X"
  val Y             = "Y"
  val Results       = "Results"
  val Params        = "Params"
  val Elt           = "Elt"
  val Sel           = "Sel"
}
