package io.joern.gosrc2cpg.parser

import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.utils.AstGenRunner.getClass
import org.slf4j.{Logger, LoggerFactory}

object ParserAst {
  private val logger                     = LoggerFactory.getLogger(getClass)
  private val QualifiedClassName: String = ParserAst.getClass.getName

  def fromString(nodeName: String, fileName: String): ParserNode = {
    try {
      val clazz = Class.forName(s"$QualifiedClassName${nodeName.stripPrefix("ast.")}$$")
      clazz.getField("MODULE$").get(clazz).asInstanceOf[ParserNode]
    } catch {
      case _ =>
        logger.warn(s"`$nodeName` AST type is not handled. We found this inside '$fileName'")
        NotHandledType
    }
  }

  sealed trait ParserNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }
  object NotHandledType      extends ParserNode
  sealed trait BaseExpr      extends ParserNode
  object BinaryExpr          extends BaseExpr
  object KeyValueExpr        extends BaseExpr
  object UnaryExpr           extends BaseExpr
  object StarExpr            extends BaseExpr
  object ParenExpr           extends BaseExpr
  object TypeAssertExpr      extends BaseExpr
  object SelectorExpr        extends BaseExpr
  object CallExpr            extends BaseExpr
  object StructType          extends BaseExpr
  object IndexExpr           extends BaseExpr
  sealed trait BaseStmt      extends ParserNode
  object BlockStmt           extends BaseStmt
  object DeclStmt            extends BaseStmt
  object AssignStmt          extends BaseStmt
  object ExprStmt            extends BaseStmt
  object IncDecStmt          extends BaseStmt
  object IfStmt              extends BaseStmt
  object SwitchStmt          extends BaseStmt
  object TypeSwitchStmt      extends BaseStmt
  object ReturnStmt          extends BaseStmt
  object ForStmt             extends BaseStmt
  object RangeStmt           extends BaseStmt
  object BranchStmt          extends BaseStmt
  object LabeledStmt         extends BaseStmt
  sealed trait BasePrimitive extends ParserNode
  object BasicLit            extends BasePrimitive
  object Ident               extends BasePrimitive
  object CompositeLit        extends BasePrimitive
  object FuncLit             extends BasePrimitive
  object File                extends ParserNode
  object GenDecl             extends ParserNode
  object ImportSpec          extends ParserNode
  object FuncDecl            extends ParserNode
  object ValueSpec           extends ParserNode
  object CaseClause          extends ParserNode
  object InterfaceType       extends ParserNode
  object FuncType            extends ParserNode
  object Ellipsis            extends ParserNode
  object Unknown             extends ParserNode
  object FieldList           extends ParserNode
  object ArrayType           extends ParserNode
  object MapType             extends ParserNode
  object ChanType            extends ParserNode
  object TypeSpec            extends ParserNode
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
  val Fun             = "Fun"
  val Fields          = "Fields"
  val TypeParams      = "TypeParams"
  val Args            = "Args"
  val Recv            = "Recv"
  val Index           = "Index"
}
