package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.astcreation.GlobalTypes.{builtinFunctions, builtinPrefix}
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.RubyNode
import io.joern.x2cpg.datastructures.Scope
import io.joern.x2cpg.datastructures.Stack.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}

trait AstCreatorHelper { this: AstCreator =>

  /* Used to track variable names and their LOCAL nodes.
   * TODO: Perhaps move this feature into a new Pass?
   */
  protected val scope: Scope[String, NewNode, NewNode] = new Scope()

  /* Used to compute a method's full name and parent.
   * TODO: port RubyScope from the deprecated frontend here?
   * */
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  /* Used if any constructors of classes are present to know if a default constructor should be generated
   * TODO: this seems too specific to add another stack, perhaps there is a better way in checking the class body. There are some possible
   * nesting edge cases which this handles better unless you recursively traverse the result of astsFor* on the class body. How common it would be in actual Ruby code is uncertain */
  protected val shouldGenerateDefaultConstructorStack: Stack[Boolean] = new Stack()
  protected def setNoDefaultConstructorForEnclosingTypeDecl: Unit = {
    shouldGenerateDefaultConstructorStack.pop()
    shouldGenerateDefaultConstructorStack.push(false)
  }

  protected def getEnclosingAstType: String     = methodAstParentStack.head.label()
  protected def getEnclosingAstFullName: String = methodAstParentStack.head.properties(PropertyNames.FULL_NAME).toString
  protected def computeClassFullName(name: String): String  = s"$getEnclosingAstFullName.$name"
  protected def computeMethodFullName(name: String): String = s"$getEnclosingAstFullName:$name"

  override def column(node: RubyNode): Option[Integer]    = node.column
  override def columnEnd(node: RubyNode): Option[Integer] = node.columnEnd
  override def line(node: RubyNode): Option[Integer]      = node.line
  override def lineEnd(node: RubyNode): Option[Integer]   = node.lineEnd
  override def code(node: RubyNode): String               = shortenCode(node.text)

  protected def isBuiltin(x: String): Boolean      = builtinFunctions.contains(x)
  protected def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"
  protected def pathSep                            = "."

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
      ">>"  -> Operators.logicalShiftRight
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
