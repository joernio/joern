package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.passes.Defines as RubyDefines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.collection.mutable
import scala.util.Try

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  import io.joern.rubysrc2cpg.deprecated.astcreation.GlobalTypes.*

  protected def line(ctx: ParserRuleContext): Option[Int] =
    Try(ctx.getStart.getLine).toOption

  protected def column(ctx: ParserRuleContext): Option[Int] =
    Try(ctx.getStart.getCharPositionInLine).toOption

  protected def lineEnd(ctx: ParserRuleContext): Option[Int] =
    Try(ctx.getStop.getLine).toOption

  protected def columnEnd(ctx: ParserRuleContext): Option[Int] =
    Try(ctx.getStop.getCharPositionInLine).toOption

  override def code(node: ParserRuleContext): String = shortenCode(text(node))

  protected def text(ctx: ParserRuleContext): String = Try {
    val a     = ctx.getStart.getStartIndex
    val b     = ctx.getStop.getStopIndex
    val intv  = new Interval(a, b)
    val input = ctx.getStart.getInputStream
    input.getText(intv)
  }.getOrElse("<empty>")

  protected def isBuiltin(x: String): Boolean = builtinFunctions.contains(x)

  protected def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"

  protected def methodsWithName(name: String): List[String] = {
    packageContext.packageTable.getMethodFullNameUsingName(methodName = name)
  }

  private def methodTableToCallNode(
    methodFullName: String,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq(),
    ctx: Option[ParserRuleContext] = None
  ): NewCall = {
    callNode(ctx.orNull, code, name, methodFullName, DispatchTypes.DYNAMIC_DISPATCH, None, Option(typeFullName))
      .dynamicTypeHintFullName(dynamicTypeHints)
  }

  /** Checks that the name is not `this` and that the method has been referred to more than just an initial assignment
    * to METHOD_REF.
    *
    * @param name
    *   the identifier name.
    * @return
    *   true if this appears to be more like a method call than an identifier.
    */
  private def isMethodCall(name: String): Boolean = {
    name != "this" && scope.numVariableReferences(name) == 0
  }

  protected def createIdentifierWithScope(
    ctx: ParserRuleContext,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq(),
    definitelyIdentifier: Boolean = false
  ): NewNode = {
    methodsWithName(name) match
      case method :: _ if !definitelyIdentifier && isMethodCall(name) =>
        methodTableToCallNode(method, name, code, typeFullName, dynamicTypeHints, Option(ctx))
      case _ =>
        val newNode = identifierNode(ctx, name, code, typeFullName, dynamicTypeHints)
        scope.addToScope(name, newNode)
        newNode
  }

  protected def createIdentifierWithScope(
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String],
    lineNumber: Option[Int],
    columnNumber: Option[Int],
    definitelyIdentifier: Boolean
  ): NewNode = {
    methodsWithName(name) match
      case method :: _ if !definitelyIdentifier && isMethodCall(name) =>
        methodTableToCallNode(method, name, code, typeFullName, dynamicTypeHints, None)
      case _ =>
        val newNode = NewIdentifier()
          .name(name)
          .code(code)
          .typeFullName(typeFullName)
          .dynamicTypeHintFullName(dynamicTypeHints)
          .lineNumber(lineNumber)
          .columnNumber(columnNumber)
        scope.addToScope(name, newNode)
        newNode
  }

  protected def createOpCall(
    node: TerminalNode,
    operation: String,
    code: String,
    typeFullName: String = RubyDefines.Any
  ): NewCall = {
    NewCall()
      .name(operation)
      .methodFullName(operation)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)
      .typeFullName(typeFullName)
      .code(code)
  }

  protected def createLiteralNode(
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq.empty,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ): NewLiteral = {
    val newLiteral = NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
    lineNumber.foreach(newLiteral.lineNumber(_))
    columnNumber.foreach(newLiteral.columnNumber(_))
    newLiteral
  }

  protected def astForAssignment(
    lhs: NewNode,
    rhs: NewNode,
    lineNumber: Option[Int] = None,
    colNumber: Option[Int] = None
  ): Ast = {
    val code = Seq(lhs, rhs).collect { case x: AstNodeNew => x.code }.mkString(" = ")
    val assignment = NewCall()
      .name(Operators.assignment)
      .methodFullName(Operators.assignment)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(colNumber)

    callAst(assignment, Seq(Ast(lhs), Ast(rhs)))
  }

  protected def createThisIdentifier(
    ctx: ParserRuleContext,
    typeFullName: String = RubyDefines.Any,
    dynamicTypeHints: List[String] = List.empty
  ): NewIdentifier =
    createIdentifierWithScope(ctx, "this", "this", typeFullName, dynamicTypeHints, true).asInstanceOf[NewIdentifier]

  protected def newFieldIdentifier(ctx: ParserRuleContext): NewFieldIdentifier = {
    val c    = code(ctx)
    val name = c.replaceAll("@", "")
    NewFieldIdentifier()
      .code(c)
      .canonicalName(name)
      .lineNumber(ctx.start.getLine)
      .columnNumber(ctx.start.getCharPositionInLine)
  }

  protected def astForFieldAccess(ctx: ParserRuleContext, baseNode: NewNode): Ast = {
    val fieldAccess =
      callNode(ctx, code(ctx), Operators.fieldAccess, Operators.fieldAccess, DispatchTypes.STATIC_DISPATCH)
    val fieldIdentifier = newFieldIdentifier(ctx)
    val astChildren     = Seq(baseNode, fieldIdentifier)
    callAst(fieldAccess, astChildren.map(Ast.apply))
  }

  protected def createMethodParameterIn(
    name: String,
    lineNumber: Option[Int] = None,
    colNumber: Option[Int] = None,
    typeFullName: String = RubyDefines.Any,
    order: Int = -1,
    index: Int = -1
  ): NewMethodParameterIn = {
    NewMethodParameterIn()
      .name(name)
      .code(name)
      .lineNumber(lineNumber)
      .typeFullName(typeFullName)
      .columnNumber(colNumber)
      .order(order)
      .index(index)
  }

  protected def getUnusedVariableNames(
    usedVariableNames: mutable.HashMap[String, Int],
    variableName: String
  ): String = {
    val counter             = usedVariableNames.get(variableName).map(_ + 1).getOrElse(0)
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.put(variableName, counter)
    currentVariableName
  }

  protected def astForControlStructure(
    parserTypeName: String,
    node: TerminalNode,
    controlStructureType: String,
    code: String
  ): Ast =
    Ast(
      NewControlStructure()
        .parserTypeName(parserTypeName)
        .controlStructureType(controlStructureType)
        .code(code)
        .lineNumber(node.lineNumber)
        .columnNumber(node.columnNumber)
    )

  protected def returnNode(node: TerminalNode, code: String): NewReturn =
    NewReturn()
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)
      .code(code)

  protected def getOperatorName(token: Token): String = token.getType match {
    case ASSIGNMENT_OPERATOR => Operators.assignment
    case DOT2                => Operators.range
    case DOT3                => Operators.range
    case EMARK               => Operators.not
    case EQ                  => Operators.assignment
    case COLON2              => RubyOperators.scopeResolution
    case DOT                 => Operators.fieldAccess
    case EQGT                => RubyOperators.keyValueAssociation
    case COLON               => RubyOperators.activeRecordAssociation
    case _                   => RubyOperators.none
  }

  implicit class TerminalNodeExt(n: TerminalNode) {

    def lineNumber: Int = n.getSymbol.getLine

    def columnNumber: Int = n.getSymbol.getCharPositionInLine

  }

}

object RubyOperators {
  val none                    = "<operator>.none"
  val patternMatch            = "<operator>.patternMatch"
  val notPatternMatch         = "<operator>.notPatternMatch"
  val scopeResolution         = "<operator>.scopeResolution"
  val defined                 = "<operator>.defined"
  val keyValueAssociation     = "<operator>.keyValueAssociation"
  val activeRecordAssociation = "<operator>.activeRecordAssociation"
  val undef                   = "<operator>.undef"
  val superKeyword            = "<operator>.super"
  val stringConcatenation     = "<operator>.stringConcatenation"
  val formattedString         = "<operator>.formatString"
  val formattedValue          = "<operator>.formatValue"
}

object GlobalTypes {
  val builtinPrefix = "__builtin"
  /* Sources:
   * https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
   * https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access.)
   */
  val builtinFunctions = Set(
    "Array",
    "Complex",
    "Float",
    "Hash",
    "Integer",
    "Rational",
    "String",
    "__callee__",
    "__dir__",
    "__method__",
    "abort",
    "at_exit",
    "autoload",
    "autoload?",
    "binding",
    "block_given?",
    "callcc",
    "caller",
    "caller_locations",
    "catch",
    "chomp",
    "chomp!",
    "chop",
    "chop!",
    // "class",
    // "clone",
    "eval",
    "exec",
    "exit",
    "exit!",
    "fail",
    "fork",
    "format",
    // "frozen?",
    "gets",
    "global_variables",
    "gsub",
    "gsub!",
    "iterator?",
    "lambda",
    "load",
    "local_variables",
    "loop",
    "open",
    "p",
    "print",
    "printf",
    "proc",
    "putc",
    "puts",
    "raise",
    "rand",
    "readline",
    "readlines",
    "require",
    "require_relative",
    "select",
    "set_trace_func",
    "sleep",
    "spawn",
    "sprintf",
    "srand",
    "sub",
    "sub!",
    "syscall",
    "system",
    "tap",
    "test",
    // "then",
    "throw",
    "trace_var",
    // "trap",
    "untrace_var",
    "warn"
    // "yield_self",
  )
}
