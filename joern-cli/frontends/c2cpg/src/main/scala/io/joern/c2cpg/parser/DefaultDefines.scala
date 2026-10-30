package io.joern.c2cpg.parser

object DefaultDefines {
  val DEFAULT_CALL_CONVENTIONS: Map[String, String] = Map(
    "__fastcall" -> "__attribute((fastcall))",
    "__cdecl"    -> "__attribute((cdecl))",
    "__pascal"   -> "__attribute((pascal))"
  )

  /** MSVC's sized integer type keywords, which the GNU dialects CDT parses do not know. Without them, a typedef such as
    * `typedef unsigned __int64 UINT64;` does not declare a type and every use of that type misparses.
    */
  val MSVC_INTEGER_TYPES: Map[String, String] =
    Map("__int8" -> "char", "__int16" -> "short", "__int32" -> "int", "__int64" -> "long long")

  /** C11's alignment specifier. CDT's C parser throws on an alignment specifier whose operand is ambiguous, such as
    * `_Alignas(sizeof(T))`, and the whole file is lost. The graph does not represent alignment, so it is defined away.
    */
  val ALIGNMENT_SPECIFIER: Map[String, String] = Map("_Alignas(x)" -> "")
}
