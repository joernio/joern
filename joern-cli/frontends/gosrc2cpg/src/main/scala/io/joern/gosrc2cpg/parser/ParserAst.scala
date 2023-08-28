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
  sealed trait BaseStmt extends ParserNode

  sealed trait BasePrimitive extends ParserNode

  object File       extends ParserNode
  object GenDecl    extends ParserNode
  object ImportSpec extends ParserNode
  object BasicLit   extends BasePrimitive
  object FuncDecl   extends ParserNode
  object BlockStmt  extends BaseStmt
  object DeclStmt   extends BaseStmt
  object ValueSpec  extends ParserNode
  object Ident      extends BasePrimitive
  object AssignStmt extends BaseStmt
  object ExprStmt   extends BaseStmt
  object BinaryExpr extends BaseExpr
  object UnaryExpr  extends BaseExpr
  object StarExpr   extends BaseExpr

  object IncDecStmt     extends BaseStmt
  object IfStmt         extends BaseStmt
  object ParenExpr      extends BaseExpr
  object SwitchStmt     extends BaseStmt
  object CaseClause     extends ParserNode
  object TypeSwitchStmt extends BaseStmt
  object TypeAssertExpr extends BaseExpr
  object InterfaceType  extends ParserNode
  object ReturnStmt     extends BaseStmt
  object FuncType       extends ParserNode
  object Ellipsis       extends ParserNode
  object SelectorExpr   extends BaseExpr
  object ForStmt        extends BaseStmt
  object CallExpr       extends BaseExpr
  object RangeStmt      extends BaseStmt
  object Unknown        extends ParserNode
  object BranchStmt     extends BaseStmt
  object LabeledStmt    extends BaseStmt
  object FieldList      extends ParserNode
  object ArrayType      extends ParserNode
  object CompositeLit   extends BasePrimitive
  object Field          extends ParserNode
  object TypeSpec       extends ParserNode
  object StructType     extends BaseExpr
}

object ParserKeys {

  val Assign          = "Assign"
  val Body            = "Body"
  val Cond            = "Cond"
  val Decl            = "Decl"
  val Decls           = "Decls"
  val Else            = "Else"
  val Init            = "Init"
  val Key             = "Key"
  val Kind            = "Kind"
  val Label           = "Label"
  val List            = "List"
  val Lhs             = "Lhs"
  val Name            = "Name"
  val Names           = "Names"
  val NodeColNo       = "node_col_no"
  val NodeColEndNo    = "node_col_no_end"
  val NodeFileName    = "node_filename"
  val NodeId          = "node_id"
  val NodeLineNo      = "node_line_no"
  val NodeLineEndNo   = "node_line_no_end"
  val NodeReferenceId = "node_reference_id"
  val NodeType        = "node_type"
  val Obj             = "Obj"
  val Op              = "Op"
  val Path            = "Path"
  val Post            = "Post"
  val Rhs             = "Rhs"
  val Specs           = "Specs"
  val Tag             = "Tag"
  val Tok             = "Tok"
  val Type            = "Type"
  val Value           = "Value"
  val Values          = "Values"
  val X               = "X"
  val Y               = "Y"
  val Results         = "Results"
  val Params          = "Params"
  val Elt             = "Elt"
  val Sel             = "Sel"
  val Elts            = "Elts"
  val Fields          = "Fields"
}
