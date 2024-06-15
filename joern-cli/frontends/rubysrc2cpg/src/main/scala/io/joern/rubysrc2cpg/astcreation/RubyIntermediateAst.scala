package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.passes.{Defines, GlobalTypes}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.annotation.tailrec

object RubyIntermediateAst {

  case class TextSpan(
    line: Option[Integer],
    column: Option[Integer],
    lineEnd: Option[Integer],
    columnEnd: Option[Integer],
    text: String
  ) {
    def spanStart(newText: String = ""): TextSpan = TextSpan(line, column, line, column, newText)
  }

  sealed class RubyNode(val span: TextSpan) {
    def line: Option[Integer] = span.line

    def column: Option[Integer] = span.column

    def lineEnd: Option[Integer] = span.lineEnd

    def columnEnd: Option[Integer] = span.columnEnd

    def text: String = span.text
  }

  implicit class RubyNodeHelper(node: RubyNode) {
    def asStatementList: StatementList = node match {
      case stmtList: StatementList => stmtList
      case _                       => StatementList(List(node))(node.span)
    }
  }

  final case class Unknown()(span: TextSpan) extends RubyNode(span)

  final case class StatementList(statements: List[RubyNode])(span: TextSpan) extends RubyNode(span) {
    override def text: String = statements.size match
      case 0 | 1 => span.text
      case _     => "(...)"

    def size: Int = statements.size
  }

  sealed trait AllowedTypeDeclarationChild

  sealed trait TypeDeclaration extends AllowedTypeDeclarationChild {
    def name: RubyNode
    def baseClass: Option[RubyNode]
    def body: RubyNode
  }

  final case class ModuleDeclaration(name: RubyNode, body: RubyNode, fields: List[RubyNode & RubyFieldIdentifier])(
    span: TextSpan
  ) extends RubyNode(span)
      with TypeDeclaration {
    def baseClass: Option[RubyNode] = None
  }

  final case class ClassDeclaration(
    name: RubyNode,
    baseClass: Option[RubyNode],
    body: RubyNode,
    fields: List[RubyNode & RubyFieldIdentifier]
  )(span: TextSpan)
      extends RubyNode(span)
      with TypeDeclaration

  sealed trait AnonymousTypeDeclaration extends RubyNode with TypeDeclaration

  final case class AnonymousClassDeclaration(name: RubyNode, baseClass: Option[RubyNode], body: RubyNode)(
    span: TextSpan
  ) extends RubyNode(span)
      with AnonymousTypeDeclaration

  final case class SingletonClassDeclaration(name: RubyNode, baseClass: Option[RubyNode], body: RubyNode)(
    span: TextSpan
  ) extends RubyNode(span)
      with AnonymousTypeDeclaration

  final case class FieldsDeclaration(fieldNames: List[RubyNode])(span: TextSpan)
      extends RubyNode(span)
      with AllowedTypeDeclarationChild {
    def hasGetter: Boolean = text.startsWith("attr_reader") || text.startsWith("attr_accessor")

    def hasSetter: Boolean = text.startsWith("attr_writer") || text.startsWith("attr_accessor")
  }

  final case class MethodDeclaration(methodName: String, parameters: List[RubyNode], body: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with AllowedTypeDeclarationChild

  final case class SingletonMethodDeclaration(
    target: RubyNode,
    methodName: String,
    parameters: List[RubyNode],
    body: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)
      with AllowedTypeDeclarationChild

  sealed trait MethodParameter {
    def name: String
  }

  final case class MandatoryParameter(name: String)(span: TextSpan) extends RubyNode(span) with MethodParameter

  final case class OptionalParameter(name: String, defaultExpression: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with MethodParameter

  sealed trait CollectionParameter extends MethodParameter

  final case class ArrayParameter(name: String)(span: TextSpan) extends RubyNode(span) with CollectionParameter

  final case class HashParameter(name: String)(span: TextSpan) extends RubyNode(span) with CollectionParameter

  final case class ProcParameter(name: String)(span: TextSpan) extends RubyNode(span) with MethodParameter

  final case class SingleAssignment(lhs: RubyNode, op: String, rhs: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class MultipleAssignment(assignments: List[SingleAssignment])(span: TextSpan) extends RubyNode(span)

  final case class SplattingRubyNode(name: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class AttributeAssignment(target: RubyNode, op: String, attributeName: String, rhs: RubyNode)(
    span: TextSpan
  ) extends RubyNode(span)

  /** Any structure that conditionally modifies the control flow of the program.
    */
  sealed trait ControlFlowExpression

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
    body: RubyNode,
    rescueClauses: List[RescueClause],
    elseClause: Option[ElseClause],
    ensureClause: Option[EnsureClause]
  )(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class RescueClause(
    exceptionClassList: Option[RubyNode],
    variables: Option[RubyNode],
    thenClause: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowClause

  final case class EnsureClause(thenClause: RubyNode)(span: TextSpan) extends RubyNode(span) with ControlFlowClause

  final case class WhileExpression(condition: RubyNode, body: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class DoWhileExpression(condition: RubyNode, body: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class UntilExpression(condition: RubyNode, body: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class IfExpression(
    condition: RubyNode,
    thenClause: RubyNode,
    elsifClauses: List[RubyNode],
    elseClause: Option[RubyNode]
  )(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class ElsIfClause(condition: RubyNode, thenClause: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowClause

  final case class ElseClause(thenClause: RubyNode)(span: TextSpan) extends RubyNode(span) with ControlFlowClause

  final case class UnlessExpression(condition: RubyNode, trueBranch: RubyNode, falseBranch: Option[RubyNode])(
    span: TextSpan
  ) extends RubyNode(span)
      with ControlFlowExpression

  final case class ForExpression(forVariable: RubyNode, iterableVariable: RubyNode, doBlock: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class CaseExpression(
    expression: Option[RubyNode],
    whenClauses: List[RubyNode],
    elseClause: Option[RubyNode]
  )(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowExpression

  final case class WhenClause(
    matchExpressions: List[RubyNode],
    matchSplatExpression: Option[RubyNode],
    thenClause: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)
      with ControlFlowClause

  final case class ReturnExpression(expressions: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  /** Represents an unqualified identifier e.g. `X`, `x`,  `@@x`, `$x`, `$<`, etc. */
  final case class SimpleIdentifier(typeFullName: Option[String] = None)(span: TextSpan)
      extends RubyNode(span)
      with RubyIdentifier
      with SingletonMethodIdentifier {
    override def toString: String = s"SimpleIdentifier(${span.text}, $typeFullName)"
  }

  /** Represents a type reference successfully determined, e.g. module A; end; A
    */
  final case class TypeIdentifier(typeFullName: String)(span: TextSpan) extends RubyNode(span) with RubyIdentifier {
    def isBuiltin: Boolean        = typeFullName.startsWith(s"<${GlobalTypes.builtinPrefix}")
    override def toString: String = s"TypeIdentifier(${span.text}, $typeFullName)"
  }

  /** Represents a InstanceFieldIdentifier e.g `@x` */
  final case class InstanceFieldIdentifier()(span: TextSpan) extends RubyNode(span) with RubyFieldIdentifier

  /** Represents a ClassFieldIdentifier e.g `@@x` */
  final case class ClassFieldIdentifier()(span: TextSpan) extends RubyNode(span) with RubyFieldIdentifier

  final case class SelfIdentifier()(span: TextSpan) extends RubyNode(span) with SingletonMethodIdentifier

  /** Represents some kind of literal expression.
    */
  sealed trait LiteralExpr {
    def typeFullName: String
  }

  /** Represents a non-interpolated literal. */
  final case class StaticLiteral(typeFullName: String)(span: TextSpan) extends RubyNode(span) with LiteralExpr {
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

  final case class DynamicLiteral(typeFullName: String, expressions: List[RubyNode])(span: TextSpan)
      extends RubyNode(span)
      with LiteralExpr

  final case class RangeExpression(lowerBound: RubyNode, upperBound: RubyNode, rangeOperator: RangeOperator)(
    span: TextSpan
  ) extends RubyNode(span)

  final case class RangeOperator(exclusive: Boolean)(span: TextSpan) extends RubyNode(span)

  final case class ArrayLiteral(elements: List[RubyNode])(span: TextSpan) extends RubyNode(span) with LiteralExpr {
    def isSymbolArray: Boolean = text.take(2).toLowerCase.startsWith("%i")

    def isStringArray: Boolean = text.take(2).toLowerCase.startsWith("%w")

    def isDynamic: Boolean = text.take(2).startsWith("%I") || text.take(2).startsWith("%W")

    def isStatic: Boolean = !isDynamic

    def typeFullName: String = Defines.getBuiltInType(Defines.Array)
  }

  final case class HashLiteral(elements: List[RubyNode])(span: TextSpan) extends RubyNode(span) with LiteralExpr {
    def typeFullName: String = Defines.getBuiltInType(Defines.Hash)
  }

  final case class Association(key: RubyNode, value: RubyNode)(span: TextSpan) extends RubyNode(span)

  /** Represents a call.
    */
  sealed trait RubyCall {
    def target: RubyNode
    def arguments: List[RubyNode]
  }

  /** Represents traditional calls, e.g. `foo`, `foo x, y`, `foo(x,y)` */
  final case class SimpleCall(target: RubyNode, arguments: List[RubyNode])(span: TextSpan)
      extends RubyNode(span)
      with RubyCall

  final case class RequireCall(
    target: RubyNode,
    argument: RubyNode,
    isRelative: Boolean = false,
    isWildCard: Boolean = false
  )(span: TextSpan)
      extends RubyNode(span)
      with RubyCall {
    def arguments: List[RubyNode] = List(argument)
    def asSimpleCall: SimpleCall  = SimpleCall(target, arguments)(span)
  }

  final case class IncludeCall(target: RubyNode, argument: RubyNode)(span: TextSpan)
      extends RubyNode(span)
      with RubyCall {
    def arguments: List[RubyNode] = List(argument)
    def asSimpleCall: SimpleCall  = SimpleCall(target, arguments)(span)
  }

  /** Represents standalone `proc { ... }` or `lambda { ... }` expressions
    */
  final case class ProcOrLambdaExpr(block: Block)(span: TextSpan) extends RubyNode(span)

  final case class YieldExpr(arguments: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  /** Represents a call with a block argument.
    */
  sealed trait RubyCallWithBlock[C <: RubyCall] extends RubyCall {

    def block: Block

    def withoutBlock: RubyNode & C
  }

  final case class SimpleCallWithBlock(target: RubyNode, arguments: List[RubyNode], block: Block)(span: TextSpan)
      extends RubyNode(span)
      with RubyCallWithBlock[SimpleCall] {
    def withoutBlock: SimpleCall = SimpleCall(target, arguments)(span)
  }

  /** Represents member calls, e.g. `x.y(z,w)` */
  final case class MemberCall(target: RubyNode, op: String, methodName: String, arguments: List[RubyNode])(
    span: TextSpan
  ) extends RubyNode(span)
      with RubyCall

  final case class MemberCallWithBlock(
    target: RubyNode,
    op: String,
    methodName: String,
    arguments: List[RubyNode],
    block: Block
  )(span: TextSpan)
      extends RubyNode(span)
      with RubyCallWithBlock[MemberCall] {
    def withoutBlock: MemberCall = MemberCall(target, op, methodName, arguments)(span)
  }

  /** Represents index accesses, e.g. `x[0]`, `self.x.y[1, 2]` */
  final case class IndexAccess(target: RubyNode, indices: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  final case class MemberAccess(target: RubyNode, op: String, memberName: String)(span: TextSpan)
      extends RubyNode(span) {
    override def toString: String = s"${target.text}.$memberName"
  }

  /** A Ruby node that instantiates objects.
    */
  sealed trait ObjectInstantiation extends RubyCall

  final case class SimpleObjectInstantiation(target: RubyNode, arguments: List[RubyNode])(span: TextSpan)
      extends RubyNode(span)
      with ObjectInstantiation

  final case class ObjectInstantiationWithBlock(target: RubyNode, arguments: List[RubyNode], block: Block)(
    span: TextSpan
  ) extends RubyNode(span)
      with ObjectInstantiation
      with RubyCallWithBlock[SimpleObjectInstantiation] {
    def withoutBlock: SimpleObjectInstantiation = SimpleObjectInstantiation(target, arguments)(span)
  }

  /** Represents a `do` or `{ .. }` (braces) block. */
  final case class Block(parameters: List[RubyNode], body: RubyNode)(span: TextSpan) extends RubyNode(span) {

    def toMethodDeclaration(name: String, parameters: Option[List[RubyNode]]): MethodDeclaration = parameters match {
      case Some(givenParameters) => MethodDeclaration(name, givenParameters, body)(span)
      case None                  => MethodDeclaration(name, this.parameters, body)(span)
    }

  }

  /** A dummy class for wrapping around `NewNode` and allowing it to integrate with RubyNode classes.
    */
  final case class DummyNode(node: NewNode)(span: TextSpan) extends RubyNode(span)

  final case class UnaryExpression(op: String, expression: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class BinaryExpression(lhs: RubyNode, op: String, rhs: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class HereDocNode(content: String)(span: TextSpan) extends RubyNode(span)

  final case class AliasStatement(oldName: String, newName: String)(span: TextSpan) extends RubyNode(span)

  final case class BreakStatement()(span: TextSpan) extends RubyNode(span)
}
