package io.joern.c2cpg.astcreation

object Defines {
  val Any: String                            = io.joern.x2cpg.Defines.Any
  val Void: String                           = "void"
  val Function: String                       = "std.function"
  val Array: String                          = "std.array"
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
  val OperatorExpressionList: String         = "<operator>.expressionList"
  val OperatorNew: String                    = "<operator>.new"
  val OperatorBracketedPrimary: String       = "<operator>.bracketedPrimary"
}
