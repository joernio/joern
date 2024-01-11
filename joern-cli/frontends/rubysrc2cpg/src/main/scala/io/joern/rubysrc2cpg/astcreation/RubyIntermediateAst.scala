package io.joern.rubysrc2cpg.astcreation

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
    def asStatementList = node match
      case stmtList: StatementList => stmtList
      case _                       => StatementList(List(node))(node.span)

  }

  final case class Unknown()(span: TextSpan) extends RubyNode(span)

  final case class StatementList(statements: List[RubyNode])(span: TextSpan) extends RubyNode(span) {
    override def text: String = statements.size match
      case 0 | 1 => span.text
      case _     => "(...)"

    def size: Int = statements.size
  }

  final case class ModuleDeclaration(moduleName: RubyNode, body: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class ClassDeclaration(className: RubyNode, baseClass: Option[RubyNode], body: RubyNode)(span: TextSpan)
      extends RubyNode(span)

  final case class FieldsDeclaration(fieldNames: List[RubyNode])(span: TextSpan) extends RubyNode(span) {
    def hasGetter: Boolean = text.startsWith("attr_reader") || text.startsWith("attr_accessor")

    def hasSetter: Boolean = text.startsWith("attr_writer") || text.startsWith("attr_accessor")
  }

  final case class MethodDeclaration(methodName: String, parameters: List[RubyNode], body: RubyNode)(span: TextSpan)
      extends RubyNode(span)

  final case class SingletonMethodDeclaration(
    target: RubyNode,
    methodName: String,
    parameters: List[RubyNode],
    body: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)

  final case class MandatoryParameter()(span: TextSpan) extends RubyNode(span)

  final case class OptionalParameter(name: RubyNode, defaultExpression: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class ArrayParameter(name: Option[String])(span: TextSpan) extends RubyNode(span)

  final case class HashParameter(name: Option[String])(span: TextSpan) extends RubyNode(span)

  final case class ProcParameter(name: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class SingleAssignment(lhs: RubyNode, op: String, rhs: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class AttributeAssignment(target: RubyNode, op: String, attributeName: String, rhs: RubyNode)(
    span: TextSpan
  ) extends RubyNode(span)

  final case class RescueExpression(
    body: RubyNode,
    rescueClauses: List[RubyNode],
    elseClause: Option[RubyNode],
    ensureClause: Option[RubyNode]
  )(span: TextSpan)
      extends RubyNode(span)

  final case class RescueClause(
    exceptionClassList: Option[RubyNode],
    assignment: Option[RubyNode],
    thenClause: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)

  final case class EnsureClause(thenClause: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class WhileExpression(condition: RubyNode, body: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class UntilExpression(condition: RubyNode, body: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class IfExpression(
    condition: RubyNode,
    thenClause: RubyNode,
    elsifClauses: List[RubyNode],
    elseClause: Option[RubyNode]
  )(span: TextSpan)
      extends RubyNode(span)

  final case class ElsIfClause(condition: RubyNode, thenClause: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class ElseClause(thenClause: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class UnlessExpression(condition: RubyNode, trueBranch: RubyNode, falseBranch: Option[RubyNode])(
    span: TextSpan
  ) extends RubyNode(span)

  final case class ConditionalExpression(condition: RubyNode, trueBranch: RubyNode, falseBranch: RubyNode)(
    span: TextSpan
  ) extends RubyNode(span)

  final case class CaseExpression(
    expression: Option[RubyNode],
    whenClauses: List[RubyNode],
    elseClause: Option[RubyNode]
  )(span: TextSpan)
      extends RubyNode(span)

  final case class WhenClause(
    matchExpressions: List[RubyNode],
    matchSplatExpression: Option[RubyNode],
    thenClause: RubyNode
  )(span: TextSpan)
      extends RubyNode(span)

  final case class ReturnExpression(expressions: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  /** Represents an unqualified identifier e.g. `X`, `x`, `@x`, `@@x`, `$x`, `$<`, etc. */
  final case class SimpleIdentifier(typeFullName: Option[String] = None)(span: TextSpan) extends RubyNode(span)

  final case class SelfIdentifier()(span: TextSpan) extends RubyNode(span)

  /** Represents a non-interpolated literal. */
  final case class StaticLiteral(typeFullName: String)(span: TextSpan) extends RubyNode(span) {
    def isSymbol: Boolean = text.startsWith(":")

    def isString: Boolean = text.startsWith("\"")

    def innerText: String = text match
      case s":'$content'" => content
      case s":$symbol"    => symbol
      case s"'$content'"  => content
      case s              => s
  }

  final case class DynamicLiteral(typeFullName: String, expressions: List[RubyNode])(span: TextSpan)
      extends RubyNode(span)

  final case class RangeExpression(lowerBound: RubyNode, upperBound: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class ArrayLiteral(elements: List[RubyNode])(span: TextSpan) extends RubyNode(span) {
    def isSymbolArray: Boolean = text.take(2).toLowerCase.startsWith("%i")

    def isStringArray: Boolean = text.take(2).toLowerCase.startsWith("%w")

    def isDynamic: Boolean = text.take(2).startsWith("%I") || text.take(2).startsWith("%W")

    def isStatic: Boolean = !isDynamic
  }

  final case class HashLiteral(elements: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  final case class Association(key: RubyNode, value: RubyNode)(span: TextSpan) extends RubyNode(span)

  /** Represents traditional calls, e.g. `foo`, `foo x, y`, `foo(x,y)` */
  final case class SimpleCall(target: RubyNode, arguments: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  final case class SimpleCallWithBlock(target: RubyNode, arguments: List[RubyNode], block: RubyNode)(span: TextSpan)
      extends RubyNode(span) {
    def withoutBlock: SimpleCall = SimpleCall(target, arguments)(span)
  }

  /** Represents member calls, e.g. `x.y(z,w)` */
  final case class MemberCall(target: RubyNode, op: String, methodName: String, arguments: List[RubyNode])(
    span: TextSpan
  ) extends RubyNode(span)

  final case class MemberCallWithBlock(
    target: RubyNode,
    op: String,
    methodName: String,
    arguments: List[RubyNode],
    block: RubyNode
  )(span: TextSpan)
      extends RubyNode(span) {
    def withoutBlock: MemberCall = MemberCall(target, op, methodName, arguments)(span)
  }

  /** Represents index accesses, e.g. `x[0]`, `self.x.y[1, 2]` */
  final case class IndexAccess(target: RubyNode, indices: List[RubyNode])(span: TextSpan) extends RubyNode(span)

  // TODO: Might be replaced by MemberCall simply?
  final case class MemberAccess(target: RubyNode, op: String, methodName: String)(span: TextSpan) extends RubyNode(span)

  /** Represents a `do` or `{ .. }` (braces) block. */
  final case class Block(parameters: List[RubyNode], body: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class UnaryExpression(op: String, expression: RubyNode)(span: TextSpan) extends RubyNode(span)

  final case class BinaryExpression(lhs: RubyNode, op: String, rhs: RubyNode)(span: TextSpan) extends RubyNode(span)
}
