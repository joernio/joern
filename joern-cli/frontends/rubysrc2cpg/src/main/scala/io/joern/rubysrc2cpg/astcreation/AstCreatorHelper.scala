package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.GlobalTypes.{builtinFunctions, builtinPrefix}
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.RubyNode
import io.joern.rubysrc2cpg.datastructures.{BlockScope, MethodLikeScope, RubyProgramSummary, RubyScope, TypeLikeScope}
import io.joern.x2cpg.datastructures.NamespaceLikeScope
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.rubysrc2cpg.passes.Defines.RubyOperators

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def computeClassFullName(name: String): String  = s"${scope.surroundingScopeFullName.head}.$name"
  protected def computeMethodFullName(name: String): String = s"${scope.surroundingScopeFullName.head}:$name"

  override def column(node: RubyNode): Option[Integer]    = node.column
  override def columnEnd(node: RubyNode): Option[Integer] = node.columnEnd
  override def line(node: RubyNode): Option[Integer]      = node.line
  override def lineEnd(node: RubyNode): Option[Integer]   = node.lineEnd
  override def code(node: RubyNode): String               = shortenCode(node.text)

  protected def isBuiltin(x: String): Boolean      = builtinFunctions.contains(x)
  protected def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"
  protected def pathSep                            = "."

  protected def handleVariableOccurrence(node: RubyNode): Ast = {
    val name       = code(node)
    val identifier = identifierNode(node, name, name, Defines.Any)
    val typeRef    = scope.tryResolveTypeReference(name)
    scope.lookupVariable(name) match {
      case None if typeRef.isDefined =>
        Ast(identifier.typeFullName(typeRef.get.name))
      case None =>
        val local = localNode(node, name, name, Defines.Any)
        scope.addToScope(name, local) match {
          case BlockScope(block) => diffGraph.addEdge(block, local, EdgeTypes.AST)
          case _                 =>
        }
        Ast(identifier).withRefEdge(identifier, local)
      case Some(local) =>
        Ast(identifier).withRefEdge(identifier, local)
    }
  }

  protected val UnaryOperatorNames: Map[String, String] = Map(
    "!"   -> Operators.logicalNot,
    "not" -> Operators.logicalNot,
    "~"   -> Operators.not,
    "+"   -> Operators.plus,
    "-"   -> Operators.minus
  )

  protected val BinaryOperatorNames: Map[String, String] =
    Map(
      "+"   -> Operators.addition,
      "-"   -> Operators.subtraction,
      "*"   -> Operators.multiplication,
      "/"   -> Operators.division,
      "%"   -> Operators.modulo,
      "**"  -> Operators.exponentiation,
      "=="  -> Operators.equals,
      "!="  -> Operators.notEquals,
      "<"   -> Operators.lessThan,
      "<="  -> Operators.lessEqualsThan,
      ">"   -> Operators.greaterThan,
      ">="  -> Operators.greaterEqualsThan,
      "<=>" -> Operators.compare,
      "&&"  -> Operators.logicalAnd,
      "and" -> Operators.logicalAnd,
      "or"  -> Operators.logicalOr,
      "||"  -> Operators.logicalOr,
      "&"   -> Operators.and,
      "|"   -> Operators.or,
      "^"   -> Operators.xor,
      "<<"  -> Operators.shiftLeft,
      ">>"  -> Operators.logicalShiftRight,
      "=~"  -> RubyOperators.regexpMatch
    )

  protected val AssignmentOperatorNames: Map[String, String] = Map(
    "="   -> Operators.assignment,
    "+="  -> Operators.assignmentPlus,
    "-="  -> Operators.assignmentMinus,
    "*="  -> Operators.assignmentMultiplication,
    "/="  -> Operators.assignmentDivision,
    "%="  -> Operators.assignmentModulo,
    "**=" -> Operators.assignmentExponentiation,
    // Strictly speaking, `a ||= b` means `a || a = b`, but I reckon we wouldn't gain much representing it that way.
    "||=" -> Operators.assignmentOr,
    "&&=" -> Operators.assignmentAnd
  )
}

// TODO: Move this to a more appropriate place?
object GlobalTypes {
  val builtinPrefix = "__builtin"

  /* Sources:
   * https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
   * https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access.)
   */
  val builtinFunctions: Set[String] = Set(
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
