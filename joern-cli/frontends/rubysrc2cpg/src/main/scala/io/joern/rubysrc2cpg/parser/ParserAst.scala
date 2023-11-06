package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyParser.{
  QuotedExpandedStringArrayLiteralContext,
  QuotedExpandedSymbolArrayLiteralContext
}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval

object ParserAst {

  def apply(ctx: ParserRuleContext): ParserNode = ParserAstCreator.create(ctx)

  /** A lightweight (intermediate) Ruby AST node built on top of an ANTLR's `ParserRuleContext`. */
  abstract class ParserNode(ctx: ParserRuleContext) {
    def line: Option[Integer]      = Option(ctx.getStart.getLine)
    def column: Option[Integer]    = Option(ctx.getStart.getCharPositionInLine)
    def lineEnd: Option[Integer]   = Option(ctx.getStop.getLine)
    def columnEnd: Option[Integer] = Option(ctx.getStop.getCharPositionInLine)
    def text: String =
      ctx.getStart.getInputStream.getText(new Interval(ctx.getStart.getStartIndex, ctx.getStop.getStopIndex))
  }

  /** Fallback node used when we don't have a suitable representation for a given `ParserRuleContext`. */
  final case class Unknown(ctx: ParserRuleContext) extends ParserNode(ctx)

  final case class StatementList(ctx: ParserRuleContext, statements: List[ParserRuleContext]) extends ParserNode(ctx) {
    override def text: String = statements.size match
      case 0 | 1 => ctx.getText
      case _     => "(...)"
    def size: Int = statements.size
  }

  final case class ModuleDeclaration(ctx: ParserRuleContext, moduleName: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)

  final case class ClassDeclaration(
    ctx: ParserRuleContext,
    className: ParserRuleContext,
    baseClass: Option[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class FieldsDeclaration(ctx: ParserRuleContext, fieldNames: List[ParserRuleContext])
      extends ParserNode(ctx) {
    def hasGetter: Boolean = ctx.getText.startsWith("attr_reader") || ctx.getText.startsWith("attr_accessor")
    def hasSetter: Boolean = ctx.getText.startsWith("attr_writer") || ctx.getText.startsWith("attr_accessor")
  }

  final case class MethodDeclaration(
    ctx: ParserRuleContext,
    methodName: String,
    parameters: List[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class SingletonMethodDeclaration(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    methodName: String,
    parameters: List[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class MandatoryParameter(ctx: ParserRuleContext) extends ParserNode(ctx)

  final case class OptionalParameter(
    ctx: ParserRuleContext,
    name: ParserRuleContext,
    defaultExpression: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class ArrayParameter(ctx: ParserRuleContext, name: Option[String]) extends ParserNode(ctx)

  final case class HashParameter(ctx: ParserRuleContext, name: Option[String]) extends ParserNode(ctx)

  final case class ProcParameter(ctx: ParserRuleContext, name: ParserRuleContext) extends ParserNode(ctx)

  final case class SingleAssignment(ctx: ParserRuleContext, lhs: ParserRuleContext, op: String, rhs: ParserRuleContext)
      extends ParserNode(ctx)

  final case class AttributeAssignment(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    attributeName: String,
    rhs: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class WhileExpression(ctx: ParserRuleContext, condition: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)

  final case class UntilExpression(ctx: ParserRuleContext, condition: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)

  final case class IfExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    thenClause: ParserRuleContext,
    elsifClauses: List[ParserRuleContext],
    elseClause: Option[ParserRuleContext]
  ) extends ParserNode(ctx)

  final case class ElsIfClause(ctx: ParserRuleContext, condition: ParserRuleContext, thenClause: ParserRuleContext)
      extends ParserNode(ctx)

  final case class ElseClause(ctx: ParserRuleContext, thenClause: ParserRuleContext) extends ParserNode(ctx)

  final case class UnlessExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    trueBranch: ParserRuleContext,
    falseBranch: Option[ParserRuleContext]
  ) extends ParserNode(ctx)

  final case class ConditionalExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    trueBranch: ParserRuleContext,
    falseBranch: ParserRuleContext
  ) extends ParserNode(ctx)

  final case class ReturnExpression(ctx: ParserRuleContext, expressions: List[ParserRuleContext])
      extends ParserNode(ctx)

  /** Represents an unqualified identifier e.g. `X`, `x`, `@x`, `@@x`, `$x`, `$<`, etc. */
  final case class SimpleIdentifier(ctx: ParserRuleContext, typeFullName: Option[String] = None) extends ParserNode(ctx)

  final case class SelfIdentifier(ctx: ParserRuleContext) extends ParserNode(ctx)

  /** Represents a non-interpolated literal. */
  final case class StaticLiteral(ctx: ParserRuleContext, typeFullName: String) extends ParserNode(ctx) {
    def isSymbol: Boolean = ctx.getText.startsWith(":")
    def isString: Boolean = ctx.getText.startsWith("\"")

    // TODO: ugly as hell.
    def innerText: String = {
      val originalText = ctx.getText
      if (originalText.startsWith(":'")) {
        originalText.drop(2).dropRight(1)
      } else if (originalText.startsWith(":")) {
        originalText.drop(1)
      } else if (originalText.startsWith("'")) {
        originalText.drop(1).dropRight(1)
      } else {
        originalText
      }
    }
  }

  final case class DynamicLiteral(ctx: ParserRuleContext, typeFullName: String, expressions: List[ParserRuleContext])
      extends ParserNode(ctx)

  final case class RangeExpression(ctx: ParserRuleContext, lowerBound: ParserRuleContext, upperBound: ParserRuleContext)
      extends ParserNode(ctx)

  final case class ArrayLiteral(ctx: ParserRuleContext, elements: List[ParserRuleContext]) extends ParserNode(ctx) {
    // TODO: ugly as hell.
    def isSymbolArray: Boolean = ctx.getText.take(2).toLowerCase.startsWith("%i")
    def isStringArray: Boolean = ctx.getText.take(2).toLowerCase.startsWith("%w")
    def isDynamic: Boolean = ctx.isInstanceOf[QuotedExpandedSymbolArrayLiteralContext] ||
      ctx.isInstanceOf[QuotedExpandedStringArrayLiteralContext]
    def isStatic: Boolean = !isDynamic
  }

  final case class HashLiteral(ctx: ParserRuleContext, elements: List[ParserRuleContext]) extends ParserNode(ctx)

  final case class Association(ctx: ParserRuleContext, key: ParserRuleContext, value: ParserRuleContext)
      extends ParserNode(ctx)

  /** Represents traditional calls, e.g. `foo`, `foo x, y`, `foo(x,y)` */
  final case class SimpleCall(ctx: ParserRuleContext, target: ParserRuleContext, arguments: List[ParserRuleContext])
      extends ParserNode(ctx)

  final case class SimpleCallWithBlock(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    arguments: List[ParserRuleContext],
    block: ParserRuleContext
  ) extends ParserNode(ctx) {
    def withoutBlock: SimpleCall = SimpleCall(ctx, target, arguments)
  }

  /** Represents member calls, e.g. `x.y(z,w)` */
  final case class MemberCall(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    methodName: String,
    arguments: List[ParserRuleContext]
  ) extends ParserNode(ctx)

  final case class MemberCallWithBlock(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    methodName: String,
    arguments: List[ParserRuleContext],
    block: ParserRuleContext
  ) extends ParserNode(ctx) {
    def withoutBlock: MemberCall = MemberCall(ctx, target, op, methodName, arguments)
  }

  /** Represents index accesses, e.g. `x[0]`, `self.x.y[1, 2]` */
  final case class IndexAccess(ctx: ParserRuleContext, target: ParserRuleContext, indices: List[ParserRuleContext])
      extends ParserNode(ctx)

  // TODO: Might be replaced by MemberCall simply?
  final case class MemberAccess(ctx: ParserRuleContext, target: ParserRuleContext, op: String, methodName: String)
      extends ParserNode(ctx)

  /** Represents a `do` or `{ .. }` (braces) block. */
  final case class Block(ctx: ParserRuleContext, parameters: List[ParserRuleContext], body: ParserRuleContext)
      extends ParserNode(ctx)

  final case class UnaryExpression(ctx: ParserRuleContext, op: String, expression: ParserRuleContext)
      extends ParserNode(ctx)

  final case class BinaryExpression(ctx: ParserRuleContext, lhs: ParserRuleContext, op: String, rhs: ParserRuleContext)
      extends ParserNode(ctx)
}
