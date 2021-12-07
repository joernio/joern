package io.joern.c2cpg.parser

object DefaultDefines {
  val DEFAULT_CALL_CONVENTIONS = Map(
    "__fastcall" -> "__attribute((__fastcall__))",
    "__cdecl" -> "__attribute((__cdecl__))",
    "__pascal" -> "__attribute((__pascal__))"
  )
}
