package io.joern.c2cpg.astcreation

object Defines {
  val Any: String                    = "ANY"
  val Void: String                   = "void"
  val Function: String               = "std.function"
  val Array: String                  = "std.array"
  val QualifiedNameSeparator: String = "::"
  val Empty                          = "<empty>"

  val OperatorPointerCall            = "<operator>.pointerCall"
  val OperatorConstructorInitializer = "<operator>.constructorInitializer"
  val OperatorTypeOf                 = "<operator>.typeOf"
  val OperatorMax                    = "<operator>.max"
  val OperatorMin                    = "<operator>.min"
  val OperatorEllipses               = "<operator>.op_ellipses"
  val OperatorUnknown                = "<operator>.unknown"
  val OperatorCall                   = "<operator>()"
  val OperatorExpressionList         = "<operator>.expressionList"
  val OperatorNew                    = "<operator>.new"
  val OperatorThrow                  = "<operator>.throw"
  val OperatorBracketedPrimary       = "<operator>.bracketedPrimary"
}
