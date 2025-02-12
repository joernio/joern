package io.joern.rubysrc2cpg.passes

import org.slf4j.LoggerFactory

object Defines {

  private val logger = LoggerFactory.getLogger(getClass)

  val Any: String          = "ANY"
  val Defined: String      = "defined"
  val Undefined: String    = "Undefined"
  val Object: String       = "Object"
  val NilClass: String     = "NilClass"
  val TrueClass: String    = "TrueClass"
  val FalseClass: String   = "FalseClass"
  val Numeric: String      = "Numeric"
  val New: String          = "new"
  val Integer: String      = "Integer"
  val Float: String        = "Float"
  val String: String       = "String"
  val Symbol: String       = "Symbol"
  val Array: String        = "Array"
  val Hash: String         = "Hash"
  val Encoding: String     = "Encoding"
  val Regexp: String       = "Regexp"
  val Lambda: String       = "lambda"
  val Proc: String         = "proc"
  val Loop: String         = "loop"
  val Self: String         = "self"
  val Super: String        = "super"
  val Rational: String     = "Rational"
  val Initialize: String   = "initialize"
  val TypeDeclBody: String = "<body>"

  val Main: String = "<main>"

  val Resolver: String = "<dependency-resolver>"

  def prefixAsKernelDefined(typeInString: String): String = {
    if (GlobalTypes.bundledClasses.contains(typeInString))
      logger.warn(s"Type '$typeInString' is considered a 'core' type, not a 'Kernel-contained' type")
    s"${GlobalTypes.kernelPrefix}.$typeInString"
  }

  def prefixAsCoreType(typeInString: String): String = {
    if (!GlobalTypes.bundledClasses.contains(typeInString))
      logger.warn(s"Type '$typeInString' not considered a 'core' type")
    s"${GlobalTypes.corePrefix}.$typeInString"
  }

  object RubyOperators {
    val backticks: String = "<operator>.backticks"
    val hashInitializer   = "<operator>.hashInitializer"
    val association       = "<operator>.association"
    val splat             = "<operator>.splat"
    val regexpMatch       = "=~"
    val regexpNotMatch    = "!~"

    val regexMethods = Set("match", "sub", "gsub")
  }
}

object GlobalTypes {
  val Kernel       = "Kernel"
  val corePrefix   = "__core"
  val kernelPrefix = s"$corePrefix.$Kernel"

  /** Source: https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
    */
  val bundledClasses: Set[String] = Set(
    "ARGF",
    "ArgumentError",
    "Array",
    "BasicObject",
    "Binding",
    "Class",
    "ClosedQueueError",
    "Comparable",
    "Complex",
    "ConditionVariable",
    "Continuation",
    "Dir",
    "ENV",
    "EOFError",
    "Encoding",
    "Encoding.CompatibilityError",
    "Encoding.Converter",
    "Encoding.ConverterNotFoundError",
    "Encoding.InvalidByteSequenceError",
    "Encoding.UndefinedConversionError",
    "EncodingError",
    "Enumerable",
    "Enumerator",
    "Enumerator.ArithmeticSequence",
    "Enumerator.Chain",
    "Enumerator.Generator",
    "Enumerator.Lazy",
    "Enumerator.Producer",
    "Enumerator.Yielder",
    "Errno",
    "Exception",
    "FalseClass",
    "Fiber",
    "Fiber.SchedulerInterface",
    "FiberError",
    "File",
    "File.Constants",
    "File.Stat",
    "FileTest",
    "Float",
    "FloatDomainError",
    "FrozenError",
    "GC",
    "GC.Profiler",
    "Hash",
    "IO",
    "IO.EAGAINWaitReadable",
    "IO.EAGAINWaitWritable",
    "IO.EINPROGRESSWaitReadable",
    "IO.EINPROGRESSWaitWritable",
    "IO.EWOULDBLOCKWaitReadable",
    "IO.EWOULDBLOCKWaitWritable",
    "IO.WaitReadable",
    "IO.WaitWritable",
    "IOError",
    "IndexError",
    "Integer",
    "Interrupt",
    Kernel,
    "KeyError",
    "LoadError",
    "LocalJumpError",
    "Marshal",
    "MatchData",
    "Math",
    "Math.DomainError",
    "Method",
    "Module",
    "Mutex",
    "NameError",
    "NilClass",
    "NoMatchingPatternError",
    "NoMemoryError",
    "NoMethodError",
    "NotImplementedError",
    "Numeric",
    "Object",
    "ObjectSpace",
    "ObjectSpace.WeakMap",
    "Pool",
    "Proc",
    "Process",
    "Process.GID",
    "Process.Status",
    "Process.Sys",
    "Process.UID",
    "Queue",
    "Ractor",
    "Ractor.ClosedError",
    "Ractor.Error",
    "Ractor.IsolationError",
    "Ractor.MovedError",
    "Ractor.MovedObject",
    "Ractor.RemoteError",
    "Ractor.UnsafeError",
    "Random",
    "Random.Formatter",
    "Range",
    "RangeError",
    "Rational",
    "Regexp",
    "RegexpError",
    "Ripper",
    "RubyVM",
    "RubyVM.AbstractSyntaxTree",
    "RubyVM.AbstractSyntaxTree.Node",
    "RubyVM.InstructionSequence",
    "RubyVM.MJIT",
    "RuntimeError",
    "ScriptError",
    "SecurityError",
    "Signal",
    "SignalException",
    "SizedQueue",
    "StandardError",
    "StopIteration",
    "String",
    "Struct",
    "Symbol",
    "SyntaxError",
    "SystemCallError",
    "SystemExit",
    "SystemStackError",
    "Thread",
    "Thread.Backtrace",
    "Thread.Backtrace.Location",
    "ThreadError",
    "ThreadGroup",
    "Time",
    "TracePoint",
    "TrueClass",
    "TypeError",
    "UnboundMethod",
    "UncaughtThrowError",
    "UnicodeNormalize",
    "Warning",
    "ZeroDivisionError",
    "fatal",
    "unknown"
  )

  /* Source: https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access) and those that may be commonly
   * shadowed.
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
    "require_all",
    "require_relative",
//    "select",
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
