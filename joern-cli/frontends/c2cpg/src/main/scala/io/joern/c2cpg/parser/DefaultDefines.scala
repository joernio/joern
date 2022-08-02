package io.joern.c2cpg.parser

object DefaultDefines {
  val DEFAULT_CALL_CONVENTIONS: Map[String, String] = Map(
    "__fastcall" -> "__attribute((fastcall))",
    "__cdecl"    -> "__attribute((cdecl))",
    "__pascal"   -> "__attribute((pascal))"
  )
}
