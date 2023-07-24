package io.joern.rubysrc2cpg.astcreation

import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewCall, NewNode}
import scala.collection.mutable
trait AstCreatorHelper { this: AstCreator =>

  import GlobalTypes._

  def isBuiltin(x: String): Boolean = builtinFunctions.contains(x)

  def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"

  def astForAssignment(lhs: NewNode, rhs: NewNode, lineNumber: Option[Integer], colNumber: Option[Integer]): Ast = {

    val code = codeOf(lhs) + " = " + codeOf(rhs)
    val callNode = NewCall()
      .name(Operators.assignment)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(colNumber)
      .methodFullName(Operators.assignment)

    callAst(callNode, Seq(Ast(lhs), Ast(rhs)))
  }

  protected def codeOf(node: NewNode): String = {
    node.asInstanceOf[AstNodeNew].code
  }

  def getUnusedVariableNames(usedVariableNames: mutable.HashMap[String, Int], variableName: String): String = {
    val counter             = usedVariableNames.get(variableName).map(_ + 1).getOrElse(0)
    val currentVariableName = s"${variableName}_$counter"
    usedVariableNames.put(variableName, counter)
    currentVariableName
  }
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
