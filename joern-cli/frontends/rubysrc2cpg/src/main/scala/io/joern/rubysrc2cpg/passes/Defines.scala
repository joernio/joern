package io.joern.rubysrc2cpg.passes

object Defines {
  val Any: String    = "ANY"
  val Object: String = "Object"

  val NilClass: String   = "NilClass"
  val TrueClass: String  = "TrueClass"
  val FalseClass: String = "FalseClass"

  val Numeric: String = "Numeric"
  val Integer: String = "Integer"
  val Float: String   = "Float"

  val String: String = "String"
  val Symbol: String = "Symbol"

  val Array: String = "Array"
  val Hash: String  = "Hash"

  val Encoding: String = "Encoding"
  val Regexp: String   = "Regexp"

  // TODO: The following shall be moved out eventually.
  val ModifierRedo: String  = "redo"
  val ModifierRetry: String = "retry"
  var ModifierNext: String  = "next"
}
