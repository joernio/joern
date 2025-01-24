package io.joern.csharpsrc2cpg.parser

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

  sealed trait BasePattern extends DotNetParserNode

  sealed trait BaseLabel extends DotNetParserNode

  sealed trait JumpStatement extends BaseStmt

  sealed trait BaseLambdaExpression extends BaseExpr

  object GlobalStatement extends BaseStmt

  object ExpressionStatement extends BaseStmt

  object NotHandledType extends DotNetParserNode

  object CompilationUnit extends BaseExpr

  object NamespaceDeclaration extends DeclarationExpr

  object FileScopedNamespaceDeclaration extends DeclarationExpr

  sealed trait DeclarationExpr extends BaseExpr

  sealed trait TypeDeclaration extends DeclarationExpr

  object ClassDeclaration extends TypeDeclaration

  object StructDeclaration extends TypeDeclaration

  object RecordDeclaration extends TypeDeclaration

  object EnumDeclaration extends TypeDeclaration

  object AnonymousObjectCreationExpression extends TypeDeclaration

  object EnumMemberDeclaration extends DeclarationExpr

  object InterfaceDeclaration extends TypeDeclaration

  object MethodDeclaration extends DeclarationExpr

  object ConstructorDeclaration extends DeclarationExpr

  object FieldDeclaration extends DeclarationExpr

  object VariableDeclaration extends DeclarationExpr

  object LocalDeclarationStatement extends DeclarationExpr

  object VariableDeclarator extends DeclarationExpr

  object SimpleLambdaExpression extends BaseLambdaExpression

  object ParenthesizedLambdaExpression extends BaseLambdaExpression

  sealed trait PatternExpr extends BaseExpr

  object IsPatternExpression extends PatternExpr

  object DeclarationPattern extends PatternExpr

  object SingleVariableDesignation extends PatternExpr

  object Designation extends PatternExpr

  sealed trait ClauseExpr extends BaseExpr

  object EqualsValueClause extends ClauseExpr

  sealed trait LiteralExpr extends BaseExpr

  object NumericLiteralExpression extends LiteralExpr
  object StringLiteralExpression  extends LiteralExpr
  object TrueLiteralExpression    extends LiteralExpr
  object FalseLiteralExpression   extends LiteralExpr
  object NullLiteralExpression    extends LiteralExpr

  object UsingDirective extends BaseExpr

  object Parameter extends BaseExpr

  sealed trait TypeExpr extends BaseExpr

  object ArrayType extends TypeExpr

  object PredefinedType extends TypeExpr

  object SimpleBaseType extends TypeExpr

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
  object SimpleAssignmentExpression      extends BinaryExpr

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

  object BracketedArgumentList extends BaseExpr

  trait MemberAccessExpr extends BaseExpr

  object SimpleMemberAccessExpression extends MemberAccessExpr

  object ThisExpression extends MemberAccessExpr

  object IfStatement extends BaseStmt

  object ElseClause extends ClauseExpr

  object ThrowStatement extends BaseStmt

  object ObjectCreationExpression extends BaseExpr

  object TryStatement extends BaseStmt

  object CatchDeclaration extends DeclarationExpr

  object CatchClause extends ClauseExpr

  object FinallyClause extends ClauseExpr

  object ForEachStatement extends BaseStmt

  object ForStatement extends BaseStmt

  object DoStatement extends BaseStmt

  object WhileStatement extends BaseStmt

  object SwitchStatement extends BaseStmt

  object SwitchSection extends BaseExpr

  object UsingStatement extends BaseStmt

  object RelationalPattern extends BasePattern

  object ConstantPattern extends BasePattern

  object CaseSwitchLabel extends BaseLabel

  object CasePatternSwitchLabel extends BaseLabel

  object DefaultSwitchLabel extends BaseLabel

  object BreakStatement extends JumpStatement

  object ContinueStatement extends JumpStatement

  object GotoStatement extends JumpStatement

  object ReturnStatement extends JumpStatement

  object LocalFunctionStatement extends DeclarationExpr with BaseStmt

  object AwaitExpression extends BaseExpr

  object PropertyDeclaration extends DeclarationExpr

  object TypeArgumentList extends BaseStmt

  object TypeParameter extends BaseStmt

  object GenericName extends BaseStmt

  object NullableType extends BaseExpr

  object ArrayInitializerExpression extends BaseExpr

  object ElementAccessExpression extends BaseExpr

  object CollectionExpression extends BaseExpr

  object ExpressionElement extends BaseExpr

  object CastExpression extends BaseExpr

  object AnonymousObjectMemberDeclarator extends DeclarationExpr

  object ConditionalExpression extends BaseExpr

  object ImplicitArrayCreationExpression extends BaseExpr

  object InterpolatedStringExpression extends BaseExpr

  object InterpolatedStringText extends BaseExpr

  object Interpolation extends BaseExpr

  object ConditionalAccessExpression extends MemberAccessExpr

  object MemberBindingExpression extends BaseExpr

  object SuppressNullableWarningExpression extends BaseExpr

  object AttributeList extends BaseExpr

  object Attribute extends BaseExpr

  object AttributeArgumentList extends BaseExpr

  object AttributeArgument extends BaseExpr

  object ParenthesizedExpression extends BaseExpr

  object Unknown extends DotNetParserNode

  object AccessorList extends DotNetParserNode

  object GetAccessorDeclaration extends DotNetParserNode

  object SetAccessorDeclaration extends DotNetParserNode

}

/** The JSON key values, in alphabetical order.
  */
object ParserKeys {

  val AccessorList              = "AccessorList"
  val Accessors                 = "Accessors"
  val AstRoot                   = "AstRoot"
  val Arguments                 = "Arguments"
  val ArgumentList              = "ArgumentList"
  val AttributeLists            = "AttributeLists"
  val Attributes                = "Attributes"
  val BaseList                  = "BaseList"
  val Body                      = "Body"
  val Block                     = "Block"
  val Catches                   = "Catches"
  val Code                      = "Code"
  val ColumnStart               = "ColumnStart"
  val ColumnEnd                 = "ColumnEnd"
  val Condition                 = "Condition"
  val Contents                  = "Contents"
  val Declaration               = "Declaration"
  val Designation               = "Designation"
  val Elements                  = "Elements"
  val ElementType               = "ElementType"
  val Else                      = "Else"
  val Expression                = "Expression"
  val ExpressionElement         = "ExpressionElement"
  val Expressions               = "Expressions"
  val ExpressionBody            = "ExpressionBody"
  val Finally                   = "Finally"
  val FileName                  = "FileName"
  val GetAccessorDeclaration    = "GetAccessorDeclaration"
  val Identifier                = "Identifier"
  val Incrementors              = "Incrementors"
  val Initializer               = "Initializer"
  val Initializers              = "Initializers"
  val Keyword                   = "Keyword"
  val Kind                      = "Kind"
  val Labels                    = "Labels"
  val Left                      = "Left"
  val LineStart                 = "LineStart"
  val LineEnd                   = "LineEnd"
  val MetaData                  = "MetaData"
  val Members                   = "Members"
  val Modifiers                 = "Modifiers"
  val Name                      = "Name"
  val NameEquals                = "NameEquals"
  val Operand                   = "Operand"
  val OperatorToken             = "OperatorToken"
  val Parameter                 = "Parameter"
  val Parameters                = "Parameters"
  val ParameterList             = "ParameterList"
  val Pattern                   = "Pattern"
  val Sections                  = "Sections"
  val SetAccessorDeclaration    = "SetAccessorDeclaration"
  val SingleVariableDesignation = "SingleVariableDesignation"
  val Statement                 = "Statement"
  val Statements                = "Statements"
  val ReturnType                = "ReturnType"
  val Right                     = "Right"
  val TextToken                 = "TextToken"
  val Type                      = "Type"
  val TypeArgumentList          = "TypeArgumentList"
  val Types                     = "Types"
  val Usings                    = "Usings"
  val Value                     = "Value"
  val Variables                 = "Variables"
  val WhenFalse                 = "WhenFalse"
  val WhenNotNull               = "WhenNotNull"
  val WhenTrue                  = "WhenTrue"
}
