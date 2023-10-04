package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.astcreation.GlobalTypes

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

  // For un-named identifiers and parameters
  val TempIdentifier = "tmp"
  val TempParameter  = "param"

  // Constructor method
  val Initialize = "initialize"

  /*
   * Fake methods created from yield blocks and their yield calls will have this suffix in their names
   */
  val YIELD_SUFFIX = "_yield"

  /*
   * This is used to mark call nodes created due to yield calls. This is set in their names at creation.
   * The appropriate name wrt the names of their actual methods is set later in them.
   */
  val UNRESOLVED_YIELD = "unresolved_yield"

  /*
   * Ruby provides a dynamic method declaration via its metaprogramming keyword `define_method`.
   */
  val DEFINE_METHOD = "define_method"

  def getBuiltInType(typeInString: String) = s"${GlobalTypes.builtinPrefix}.$typeInString"
}
