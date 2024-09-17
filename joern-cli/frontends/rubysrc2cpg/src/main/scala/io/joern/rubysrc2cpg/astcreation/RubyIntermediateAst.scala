package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.{AllowedTypeDeclarationChild, RubyStatement}
import io.joern.rubysrc2cpg.passes.{Defines, GlobalTypes}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.annotation.tailrec

object RubyIntermediateAst {

  case class TextSpan(
    line: Option[Int],
    column: Option[Int],
    lineEnd: Option[Int],
    columnEnd: Option[Int],
    offset: Option[(Int, Int)],
    text: String
  ) {
    def spanStart(newText: String = ""): TextSpan = TextSpan(line, column, line, column, offset, newText)
  }

  /** Most-if-not-all constructs in Ruby evaluate to some value, so we name the base class `RubyExpression`.
    */
  sealed class RubyExpression(val span: TextSpan) {
    def line: Option[Int] = span.line

    def column: Option[Int] = span.column

    def lineEnd: Option[Int] = span.lineEnd

    def columnEnd: Option[Int] = span.columnEnd

    def offset: Option[(Int, Int)] = span.offset

    def text: String = span.text
  }

  /** Ruby statements evaluate to some value (and thus are expressions), but also perform some operation, e.g.,
    * assignments, method definitions, etc.
    */
  sealed trait RubyStatement extends RubyExpression

  implicit class RubyExpressionHelper(node: RubyExpression) {
    def asStatementList: StatementList = node match {
      case stmtList: StatementList => stmtList
      case _                       => StatementList(List(node))(node.span)
    }
  }

  final case class Unknown()(span: TextSpan) extends RubyExpression(span)

  final case class StatementList(statements: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with RubyStatement {
    override def text: String = statements.size match
      case 0 | 1 => span.text
      case _     => "(...)"

    def size: Int = statements.size
  }

  final case class SingletonStatementList(statements: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with RubyStatement {
    override def text: String = statements.size match
      case 0 | 1 => span.text
      case _     => "(...)"

    def size: Int = statements.size
  }

  sealed trait AllowedTypeDeclarationChild

  sealed trait TypeDeclaration extends AllowedTypeDeclarationChild with RubyStatement {
    def name: RubyExpression
    def baseClass: Option[RubyExpression]
    def body: RubyExpression
    def bodyMemberCall: Option[TypeDeclBodyCall]
  }

  sealed trait NamespaceDeclaration extends RubyStatement {
    def namespaceParts: Option[List[String]]
  }

  final case class ModuleDeclaration(
    name: RubyExpression,
    body: RubyExpression,
    fields: List[RubyExpression & RubyFieldIdentifier],
    bodyMemberCall: Option[TypeDeclBodyCall],
    namespaceParts: Option[List[String]]
  )(span: TextSpan)
      extends RubyExpression(span)
      with TypeDeclaration
      with NamespaceDeclaration {
    def baseClass: Option[RubyExpression] = None
  }

  final case class ClassDeclaration(
    name: RubyExpression,
    baseClass: Option[RubyExpression],
    body: RubyExpression,
    fields: List[RubyExpression & RubyFieldIdentifier],
    bodyMemberCall: Option[TypeDeclBodyCall],
    namespaceParts: Option[List[String]]
  )(span: TextSpan)
      extends RubyExpression(span)
      with TypeDeclaration
      with NamespaceDeclaration

  sealed trait AnonymousTypeDeclaration extends RubyExpression with TypeDeclaration

  final case class AnonymousClassDeclaration(
    name: RubyExpression,
    baseClass: Option[RubyExpression],
    body: RubyExpression,
    bodyMemberCall: Option[TypeDeclBodyCall] = None
  )(span: TextSpan)
      extends RubyExpression(span)
      with AnonymousTypeDeclaration

  final case class SingletonClassDeclaration(
    name: RubyExpression,
    baseClass: Option[RubyExpression],
    body: RubyExpression,
    bodyMemberCall: Option[TypeDeclBodyCall] = None
  )(span: TextSpan)
      extends RubyExpression(span)
      with AnonymousTypeDeclaration

  final case class FieldsDeclaration(fieldNames: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with AllowedTypeDeclarationChild {
    def hasGetter: Boolean = text.startsWith("attr_reader") || text.startsWith("attr_accessor")

    def hasSetter: Boolean = text.startsWith("attr_writer") || text.startsWith("attr_accessor")
  }

  sealed trait ProcedureDeclaration extends RubyStatement {
    def methodName: String
    def parameters: List[RubyExpression]
    def body: RubyExpression
  }

  final case class MethodDeclaration(methodName: String, parameters: List[RubyExpression], body: RubyExpression)(
    span: TextSpan
  ) extends RubyExpression(span)
      with ProcedureDeclaration
      with AllowedTypeDeclarationChild

  final case class SingletonMethodDeclaration(
    target: RubyExpression,
    methodName: String,
    parameters: List[RubyExpression],
    body: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)
      with ProcedureDeclaration
      with AllowedTypeDeclarationChild

  final case class SingletonObjectMethodDeclaration(
    methodName: String,
    parameters: List[RubyExpression],
    body: RubyExpression,
    baseClass: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)
      with ProcedureDeclaration

  sealed trait MethodParameter {
    def name: String
  }

  final case class MandatoryParameter(name: String)(span: TextSpan) extends RubyExpression(span) with MethodParameter

  final case class OptionalParameter(name: String, defaultExpression: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with MethodParameter

  final case class GroupedParameter(
    name: String,
    tmpParam: RubyExpression,
    multipleAssignment: GroupedParameterDesugaring
  )(span: TextSpan)
      extends RubyExpression(span)
      with MethodParameter

  sealed trait CollectionParameter extends MethodParameter

  final case class ArrayParameter(name: String)(span: TextSpan) extends RubyExpression(span) with CollectionParameter

  final case class HashParameter(name: String)(span: TextSpan) extends RubyExpression(span) with CollectionParameter

  final case class ProcParameter(name: String)(span: TextSpan) extends RubyExpression(span) with MethodParameter

  final case class SingleAssignment(lhs: RubyExpression, op: String, rhs: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with RubyStatement

  trait MultipleAssignment extends RubyStatement {
    def assignments: List[SingleAssignment]
  }

  final case class DefaultMultipleAssignment(assignments: List[SingleAssignment])(span: TextSpan)
      extends RubyExpression(span)
      with MultipleAssignment

  final case class GroupedParameterDesugaring(assignments: List[SingleAssignment])(span: TextSpan)
      extends RubyExpression(span)
      with MultipleAssignment

  final case class SplattingRubyNode(target: RubyExpression)(span: TextSpan) extends RubyExpression(span)

  final case class AttributeAssignment(
    target: RubyExpression,
    op: String,
    attributeName: String,
    assignmentOperator: String,
    rhs: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)

  /** Any structure that conditionally modifies the control flow of the program. These also behave as statements.
    */
  sealed trait ControlFlowStatement extends RubyStatement

  /** A control structure's clause, which may contain an additional control structures.
    */
  sealed trait ControlFlowClause

  /** Any structure that is an Identifier, except self. e.g. `a`, `@a`, `@@a`
    */
  sealed trait RubyIdentifier

  /** Ruby Instance or Class Variable Identifiers: `@a`, `@@a`
    */
  sealed trait RubyFieldIdentifier extends RubyIdentifier

  sealed trait SingletonMethodIdentifier

  final case class RescueExpression(
    body: RubyExpression,
    rescueClauses: List[RescueClause],
    elseClause: Option[ElseClause],
    ensureClause: Option[EnsureClause]
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class RescueClause(
    exceptionClassList: Option[RubyExpression],
    variables: Option[RubyExpression],
    thenClause: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowClause

  final case class EnsureClause(thenClause: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowClause

  final case class WhileExpression(condition: RubyExpression, body: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class DoWhileExpression(condition: RubyExpression, body: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class UntilExpression(condition: RubyExpression, body: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class IfExpression(
    condition: RubyExpression,
    thenClause: RubyExpression,
    elsifClauses: List[RubyExpression],
    elseClause: Option[RubyExpression]
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement
      with RubyStatement

  final case class ElsIfClause(condition: RubyExpression, thenClause: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowClause

  final case class ElseClause(thenClause: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowClause

  final case class UnlessExpression(
    condition: RubyExpression,
    trueBranch: RubyExpression,
    falseBranch: Option[RubyExpression]
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class ForExpression(
    forVariable: RubyExpression,
    iterableVariable: RubyExpression,
    doBlock: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class CaseExpression(
    expression: Option[RubyExpression],
    whenClauses: List[RubyExpression],
    elseClause: Option[RubyExpression]
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowStatement

  final case class WhenClause(
    matchExpressions: List[RubyExpression],
    matchSplatExpression: Option[RubyExpression],
    thenClause: RubyExpression
  )(span: TextSpan)
      extends RubyExpression(span)
      with ControlFlowClause

  final case class NextExpression()(span: TextSpan) extends RubyExpression(span) with ControlFlowStatement

  final case class BreakExpression()(span: TextSpan) extends RubyExpression(span) with ControlFlowStatement

  final case class ReturnExpression(expressions: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with RubyStatement

  /** Represents an unqualified identifier e.g. `X`, `x`,  `@@x`, `$x`, `$<`, etc. */
  final case class SimpleIdentifier(typeFullName: Option[String] = None)(span: TextSpan)
      extends RubyExpression(span)
      with RubyIdentifier
      with SingletonMethodIdentifier {
    override def toString: String = s"SimpleIdentifier(${span.text}, $typeFullName)"
  }

  /** Represents a type reference successfully determined, e.g. module A; end; A
    */
  final case class TypeIdentifier(typeFullName: String)(span: TextSpan)
      extends RubyExpression(span)
      with RubyIdentifier {
    def isBuiltin: Boolean        = typeFullName.startsWith(GlobalTypes.builtinPrefix)
    override def toString: String = s"TypeIdentifier(${span.text}, $typeFullName)"
  }

  /** Represents a InstanceFieldIdentifier e.g `@x` */
  final case class InstanceFieldIdentifier()(span: TextSpan) extends RubyExpression(span) with RubyFieldIdentifier

  /** Represents a ClassFieldIdentifier e.g `@@x` */
  final case class ClassFieldIdentifier()(span: TextSpan) extends RubyExpression(span) with RubyFieldIdentifier

  final case class SelfIdentifier()(span: TextSpan) extends RubyExpression(span) with SingletonMethodIdentifier

  /** Represents some kind of literal expression.
    */
  sealed trait LiteralExpr {
    def typeFullName: String
  }

  /** Represents a non-interpolated literal. */
  final case class StaticLiteral(typeFullName: String)(span: TextSpan) extends RubyExpression(span) with LiteralExpr {
    def isSymbol: Boolean = text.startsWith(":")

    def isString: Boolean = text.startsWith("\"") || text.startsWith("'")

    def innerText: String = {
      val strRegex = "['\"]([./:]{0,3}[\\w\\d_-]+)(?:\\.rb)?['\"]".r
      text match {
        case s":'$content'"                       => content
        case s":$symbol"                          => symbol
        case strRegex(content) if content != null => content
        case s                                    => s
      }
    }
  }

  final case class DynamicLiteral(typeFullName: String, expressions: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with LiteralExpr

  final case class RangeExpression(
    lowerBound: RubyExpression,
    upperBound: RubyExpression,
    rangeOperator: RangeOperator
  )(span: TextSpan)
      extends RubyExpression(span)

  final case class RangeOperator(exclusive: Boolean)(span: TextSpan) extends RubyExpression(span)

  final case class ArrayLiteral(elements: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with LiteralExpr {
    def isSymbolArray: Boolean = text.take(2).toLowerCase.startsWith("%i")

    def isStringArray: Boolean = text.take(2).toLowerCase.startsWith("%w")

    def isDynamic: Boolean = text.take(2).startsWith("%I") || text.take(2).startsWith("%W")

    def isStatic: Boolean = !isDynamic

    def typeFullName: String = Defines.getBuiltInType(Defines.Array)
  }

  final case class HashLiteral(elements: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with LiteralExpr {
    def typeFullName: String = Defines.getBuiltInType(Defines.Hash)
  }

  final case class Association(key: RubyExpression, value: RubyExpression)(span: TextSpan) extends RubyExpression(span)

  /** Represents a call.
    */
  sealed trait RubyCall {
    def target: RubyExpression
    def arguments: List[RubyExpression]
  }

  /** Represents traditional calls, e.g. `foo`, `foo x, y`, `foo(x,y)` */
  final case class SimpleCall(target: RubyExpression, arguments: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with RubyCall

  final case class RequireCall(
    target: RubyExpression,
    argument: RubyExpression,
    isRelative: Boolean = false,
    isWildCard: Boolean = false
  )(span: TextSpan)
      extends RubyExpression(span)
      with RubyCall {
    def arguments: List[RubyExpression] = List(argument)
    def asSimpleCall: SimpleCall        = SimpleCall(target, arguments)(span)
  }

  final case class IncludeCall(target: RubyExpression, argument: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with RubyCall {
    def arguments: List[RubyExpression] = List(argument)
    def asSimpleCall: SimpleCall        = SimpleCall(target, arguments)(span)
  }

  final case class RaiseCall(target: RubyExpression, arguments: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with RubyCall

  sealed trait AccessModifier extends AllowedTypeDeclarationChild {
    def toSimpleIdentifier: SimpleIdentifier
  }

  final case class PublicModifier()(span: TextSpan) extends RubyExpression(span) with AccessModifier {
    override def toSimpleIdentifier: SimpleIdentifier = SimpleIdentifier(None)(span)
  }

  final case class PrivateModifier()(span: TextSpan) extends RubyExpression(span) with AccessModifier {
    override def toSimpleIdentifier: SimpleIdentifier = SimpleIdentifier(None)(span)
  }

  final case class ProtectedModifier()(span: TextSpan) extends RubyExpression(span) with AccessModifier {
    override def toSimpleIdentifier: SimpleIdentifier = SimpleIdentifier(None)(span)
  }

  /** Represents standalone `proc { ... }` or `lambda { ... }` expressions
    */
  final case class ProcOrLambdaExpr(block: Block)(span: TextSpan) extends RubyExpression(span)

  final case class YieldExpr(arguments: List[RubyExpression])(span: TextSpan) extends RubyExpression(span)

  /** Represents a call with a block argument.
    */
  sealed trait RubyCallWithBlock[C <: RubyCall] extends RubyCall {

    def block: Block

    def withoutBlock: RubyExpression & C
  }

  final case class SimpleCallWithBlock(target: RubyExpression, arguments: List[RubyExpression], block: Block)(
    span: TextSpan
  ) extends RubyExpression(span)
      with RubyCallWithBlock[SimpleCall] {
    def withoutBlock: SimpleCall = SimpleCall(target, arguments)(span)
  }

  /** Represents member calls, e.g. `x.y(z,w)` */
  final case class MemberCall(target: RubyExpression, op: String, methodName: String, arguments: List[RubyExpression])(
    span: TextSpan
  ) extends RubyExpression(span)
      with RubyCall

  /** Special class for `<body>` calls of type decls.
    */
  final case class TypeDeclBodyCall(target: RubyExpression, typeName: String)(span: TextSpan)
      extends RubyExpression(span)
      with RubyCall {

    def toMemberCall: MemberCall = MemberCall(target, op, Defines.TypeDeclBody, arguments)(span)

    def arguments: List[RubyExpression] = Nil

    def op: String = "::"
  }

  final case class MemberCallWithBlock(
    target: RubyExpression,
    op: String,
    methodName: String,
    arguments: List[RubyExpression],
    block: Block
  )(span: TextSpan)
      extends RubyExpression(span)
      with RubyCallWithBlock[MemberCall] {
    def withoutBlock: MemberCall = MemberCall(target, op, methodName, arguments)(span)
  }

  /** Represents index accesses, e.g. `x[0]`, `self.x.y[1, 2]` */
  final case class IndexAccess(target: RubyExpression, indices: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)

  final case class MemberAccess(target: RubyExpression, op: String, memberName: String)(span: TextSpan)
      extends RubyExpression(span) {
    override def toString: String = s"${target.text}.$memberName"
  }

  /** A Ruby node that instantiates objects.
    */
  sealed trait ObjectInstantiation extends RubyCall

  final case class SimpleObjectInstantiation(target: RubyExpression, arguments: List[RubyExpression])(span: TextSpan)
      extends RubyExpression(span)
      with ObjectInstantiation

  final case class ObjectInstantiationWithBlock(target: RubyExpression, arguments: List[RubyExpression], block: Block)(
    span: TextSpan
  ) extends RubyExpression(span)
      with ObjectInstantiation
      with RubyCallWithBlock[SimpleObjectInstantiation] {
    def withoutBlock: SimpleObjectInstantiation = SimpleObjectInstantiation(target, arguments)(span)
  }

  /** Represents a `do` or `{ .. }` (braces) block. */
  final case class Block(parameters: List[RubyExpression], body: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)
      with RubyStatement {

    def toMethodDeclaration(name: String, parameters: Option[List[RubyExpression]]): MethodDeclaration =
      parameters match {
        case Some(givenParameters) => MethodDeclaration(name, givenParameters, body)(span)
        case None                  => MethodDeclaration(name, this.parameters, body)(span)
      }
  }

  /** A dummy class for wrapping around `NewNode` and allowing it to integrate with RubyNode classes.
    */
  final case class DummyNode(node: NewNode)(span: TextSpan) extends RubyExpression(span)

  final case class UnaryExpression(op: String, expression: RubyExpression)(span: TextSpan) extends RubyExpression(span)

  final case class BinaryExpression(lhs: RubyExpression, op: String, rhs: RubyExpression)(span: TextSpan)
      extends RubyExpression(span)

  final case class HereDocNode(content: String)(span: TextSpan) extends RubyExpression(span)

  final case class AliasStatement(oldName: String, newName: String)(span: TextSpan) extends RubyExpression(span)

}
