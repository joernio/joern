package io.joern.csharpsrc2cpg.parser

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.BaseExpr
import org.slf4j.LoggerFactory

object DotNetJsonAst {

  private val logger                     = LoggerFactory.getLogger(getClass)
  private val QualifiedClassName: String = DotNetJsonAst.getClass.getName

  def fromString(nodeName: String, fileName: Option[String] = None): DotNetParserNode = {
    try {
      val clazz = Class.forName(s"$QualifiedClassName${nodeName.stripPrefix("ast.")}$$")
      clazz.getField("MODULE$").get(clazz).asInstanceOf[DotNetParserNode]
    } catch {
      case _: Throwable =>
        logger.warn(
          s"`$nodeName` AST type is not handled.${fileName.map(x => s" We found this inside '$x'").getOrElse("")}"
        )
        NotHandledType
    }
  }

  sealed trait DotNetParserNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  sealed trait BaseExpr extends DotNetParserNode
  sealed trait BaseStmt extends DotNetParserNode

  object GlobalStatement     extends BaseStmt
  object ExpressionStatement extends BaseExpr
  object NotHandledType      extends DotNetParserNode

  object CompilationUnit extends BaseExpr

  object NamespaceDeclaration extends BaseExpr

  sealed trait DeclarationExpr extends BaseExpr

  object ClassDeclaration extends DeclarationExpr

  object MethodDeclaration extends DeclarationExpr

  object FieldDeclaration extends DeclarationExpr

  object VariableDeclaration extends DeclarationExpr

  object LocalDeclarationStatement extends DeclarationExpr

  object VariableDeclarator extends DeclarationExpr

  sealed trait ClauseExpr extends BaseExpr

  object EqualsValueClause extends ClauseExpr

  sealed trait LiteralExpr extends BaseExpr

  object NumericLiteralExpression extends LiteralExpr
  object StringLiteralExpression  extends LiteralExpr

  object UsingDirective extends BaseExpr

  object Parameter extends BaseExpr

  sealed trait TypeExpr extends BaseExpr

  object ArrayType extends TypeExpr

  object PredefinedType extends TypeExpr

  object Block extends BaseExpr

  sealed trait IdentifierNode extends BaseExpr

  object IdentifierName extends IdentifierNode

  object QualifiedName extends IdentifierNode

  sealed trait UnaryExpr         extends BaseExpr
  object PostIncrementExpression extends UnaryExpr
  object PostDecrementExpression extends UnaryExpr
  object PreIncrementExpression  extends UnaryExpr
  object PreDecrementExpression  extends UnaryExpr
  object UnaryPlusExpression     extends UnaryExpr
  object UnaryMinusExpression    extends UnaryExpr
  object BitwiseNotExpression    extends UnaryExpr
  object LogicalNotExpression    extends UnaryExpr
  object AddressOfExpression     extends UnaryExpr

  sealed trait BinaryExpr                extends BaseExpr
  object AddExpression                   extends BinaryExpr
  object SubtractExpression              extends BinaryExpr
  object MultiplyExpression              extends BinaryExpr
  object DivideExpression                extends BinaryExpr
  object ModuloExpression                extends BinaryExpr
  object EqualsExpression                extends BinaryExpr
  object NotEqualsExpression             extends BinaryExpr
  object LogicalAndExpression            extends BinaryExpr
  object LogicalOrExpression             extends BinaryExpr
  object AddAssignmentExpression         extends BinaryExpr
  object SubtractAssignmentExpression    extends BinaryExpr
  object MultiplyAssignmentExpression    extends BinaryExpr
  object DivideAssignmentExpression      extends BinaryExpr
  object ModuloAssignmentExpression      extends BinaryExpr
  object AndAssignmentExpression         extends BinaryExpr
  object OrAssignmentExpression          extends BinaryExpr
  object ExclusiveOrAssignmentExpression extends BinaryExpr
  object RightShiftAssignmentExpression  extends BinaryExpr
  object LeftShiftAssignmentExpression   extends BinaryExpr

  object GreaterThanExpression        extends BinaryExpr
  object LessThanExpression           extends BinaryExpr
  object GreaterThanOrEqualExpression extends BinaryExpr
  object LessThanOrEqualExpression    extends BinaryExpr
  object BitwiseAndExpression         extends BinaryExpr
  object BitwiseOrExpression          extends BinaryExpr
  object ExclusiveOrExpression        extends BinaryExpr

  object InvocationExpression extends BaseExpr

  object Argument extends BaseExpr

  object ArgumentList extends BaseExpr

  trait MemberAccessExpr extends BaseExpr

  object SimpleMemberAccessExpression extends MemberAccessExpr
}

/** The JSON key values, in alphabetical order.
  */
object ParserKeys {

  val AstRoot       = "AstRoot"
  val Arguments     = "Arguments"
  val ArgumentList  = "ArgumentList"
  val Body          = "Body"
  val Code          = "Code"
  val ColumnStart   = "ColumnStart"
  val ColumnEnd     = "ColumnEnd"
  val Declaration   = "Declaration"
  val ElementType   = "ElementType"
  val Expression    = "Expression"
  val FileName      = "FileName"
  val Identifier    = "Identifier"
  val Initializer   = "Initializer"
  val Keyword       = "Keyword"
  val Kind          = "Kind"
  val Left          = "Left"
  val LineStart     = "LineStart"
  val LineEnd       = "LineEnd"
  val MetaData      = "MetaData"
  val Members       = "Members"
  val Modifiers     = "Modifiers"
  val Name          = "Name"
  val Operand       = "Operand"
  val OperatorToken = "OperatorToken"
  val Parameters    = "Parameters"
  val ParameterList = "ParameterList"
  val Statement     = "Statement"
  val Statements    = "Statements"
  val ReturnType    = "ReturnType"
  val Right         = "Right"
  val Type          = "Type"
  val Usings        = "Usings"
  val Value         = "Value"
  val Variables     = "Variables"

}
