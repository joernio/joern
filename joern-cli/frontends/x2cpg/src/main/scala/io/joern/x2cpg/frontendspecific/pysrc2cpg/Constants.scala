package io.joern.x2cpg.frontendspecific.pysrc2cpg

object Constants {
  val builtinPrefix = "__builtin."

  val ANY                = "ANY"
  val GLOBAL_NAMESPACE   = "<global>"
  val builtinStrType     = s"${builtinPrefix}str"
  val builtinBytesType   = s"${builtinPrefix}bytes"
  val builtinIntType     = s"${builtinPrefix}int"
  val builtinFloatType   = s"${builtinPrefix}float"
  val builtinComplexType = s"${builtinPrefix}complex"
}
