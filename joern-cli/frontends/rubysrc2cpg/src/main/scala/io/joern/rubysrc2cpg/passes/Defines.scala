package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.astcreation.GlobalTypes

object Defines {
  val Any: String        = "ANY"
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

  def getBuiltInType(typeInString: String) = s"${GlobalTypes.builtinPrefix}.$typeInString"

  object RubyOperators {
    val hashInitializer = "<operator>.hashInitializer"
    val association     = "<operator>.association"
  }
}
