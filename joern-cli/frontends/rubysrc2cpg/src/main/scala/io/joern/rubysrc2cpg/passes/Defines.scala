package io.joern.rubysrc2cpg.passes

object Defines {

  val Any: String        = "ANY"
  val Undefined: String  = "Undefined"
  val Object: String     = "Object"
  val NilClass: String   = "NilClass"
  val TrueClass: String  = "TrueClass"
  val FalseClass: String = "FalseClass"
  val Numeric: String    = "Numeric"
  val Integer: String    = "Integer"
  val Float: String      = "Float"
  val String: String     = "String"
  val Symbol: String     = "Symbol"
  val Array: String      = "Array"
  val Hash: String       = "Hash"
  val Encoding: String   = "Encoding"
  val Regexp: String     = "Regexp"
  val Lambda: String     = "lambda"
  val Proc: String       = "proc"
  val Loop: String       = "loop"
  val Self: String       = "self"

  val Program: String = ":program"

  val Resolver: String = "<dependency-resolver>"

  val AnonymousProcParameter = "<anonymous-proc-param>"

  def getBuiltInType(typeInString: String) = s"${GlobalTypes.kernelPrefix}.$typeInString"

  object RubyOperators {
    val hashInitializer = "<operator>.hashInitializer"
    val association     = "<operator>.association"
    val splat           = "<operator>.splat"
    val regexpMatch     = "=~"
    val regexpNotMatch  = "!~"
  }
}

object GlobalTypes {
  val Kernel        = "Kernel"
  val builtinPrefix = "__builtin"
  val kernelPrefix  = s"<$builtinPrefix.$Kernel>"

  /** Source: https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
    */
  val bundledClasses: Set[String] = Set(
    "Comparable",
    "Enumerable",
    "Errno",
    "FileTest",
    "GC",
    Kernel,
    "Marshal",
    "Math",
    "ObjectSpace",
    "Precision",
    "Process"
  )

  /* Source: https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access.)
   */
  val kernelFunctions: Set[String] = Set(
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
