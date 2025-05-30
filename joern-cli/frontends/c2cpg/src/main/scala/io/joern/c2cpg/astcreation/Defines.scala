package io.joern.c2cpg.astcreation

object Defines {
  val Any: String                            = io.joern.x2cpg.Defines.Any
  val UnresolvedNamespace: String            = io.joern.x2cpg.Defines.UnresolvedNamespace
  val Void: String                           = "void"
  val This: String                           = "this"
  val Function: String                       = "std.function"
  val Array: String                          = "std.array"
  val Iterator: String                       = "std.iterator"
  val Long: String                           = "long"
  val LongLong: String                       = "longlong"
  val Short: String                          = "short"
  val QualifiedNameSeparator: String         = "::"
  val Auto: String                           = "auto"
  val OperatorPointerCall: String            = "<operator>.pointerCall"
  val OperatorConstructorInitializer: String = "<operator>.constructorInitializer"
  val OperatorTypeOf: String                 = "<operator>.typeOf"
  val OperatorMax: String                    = "<operator>.max"
  val OperatorMin: String                    = "<operator>.min"
  val OperatorEllipses: String               = "<operator>.op_ellipses"
  val OperatorUnknown: String                = "<operator>.unknown"
  val OperatorCall: String                   = "<operator>()"
  val OperatorNew: String                    = "<operator>.new"
  val DuplicateSuffix                        = "<duplicate>"
  val ConstSuffix                            = "<const>"
  val GlobalTag                              = "<global>"
  val UnknownTag                             = "<unknown>"
}
