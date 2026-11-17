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

  /** The macros through which compilers' headers name standard types, such as `__UINT32_TYPE__` for `uint32_t` in GCC's
    * `stdint-gcc.h`. Compilers predefine them and CDT does not, so without them these types are unresolved wherever
    * such a header declares them. The values are those of GCC 13 for 64-bit Arm Linux, fixed so that they do not depend
    * on the compiler of the machine that builds the graph.
    */
  private[parser] val STANDARD_TYPE_MACROS: Map[String, String] = Map(
    "__CHAR16_TYPE__"       -> "short unsigned int",
    "__CHAR32_TYPE__"       -> "unsigned int",
    "__INT16_TYPE__"        -> "short int",
    "__INT32_TYPE__"        -> "int",
    "__INT64_TYPE__"        -> "long int",
    "__INT8_TYPE__"         -> "signed char",
    "__INTMAX_TYPE__"       -> "long int",
    "__INTPTR_TYPE__"       -> "long int",
    "__INT_FAST16_TYPE__"   -> "long int",
    "__INT_FAST32_TYPE__"   -> "long int",
    "__INT_FAST64_TYPE__"   -> "long int",
    "__INT_FAST8_TYPE__"    -> "signed char",
    "__INT_LEAST16_TYPE__"  -> "short int",
    "__INT_LEAST32_TYPE__"  -> "int",
    "__INT_LEAST64_TYPE__"  -> "long int",
    "__INT_LEAST8_TYPE__"   -> "signed char",
    "__PTRDIFF_TYPE__"      -> "long int",
    "__SIG_ATOMIC_TYPE__"   -> "int",
    "__SIZE_TYPE__"         -> "long unsigned int",
    "__UINT16_TYPE__"       -> "short unsigned int",
    "__UINT32_TYPE__"       -> "unsigned int",
    "__UINT64_TYPE__"       -> "long unsigned int",
    "__UINT8_TYPE__"        -> "unsigned char",
    "__UINTMAX_TYPE__"      -> "long unsigned int",
    "__UINTPTR_TYPE__"      -> "long unsigned int",
    "__UINT_FAST16_TYPE__"  -> "long unsigned int",
    "__UINT_FAST32_TYPE__"  -> "long unsigned int",
    "__UINT_FAST64_TYPE__"  -> "long unsigned int",
    "__UINT_FAST8_TYPE__"   -> "unsigned char",
    "__UINT_LEAST16_TYPE__" -> "short unsigned int",
    "__UINT_LEAST32_TYPE__" -> "unsigned int",
    "__UINT_LEAST64_TYPE__" -> "long unsigned int",
    "__UINT_LEAST8_TYPE__"  -> "unsigned char",
    "__WCHAR_TYPE__"        -> "unsigned int",
    "__WINT_TYPE__"         -> "unsigned int"
  )

  /** C11's alignment specifier. CDT's C parser throws on an alignment specifier whose operand is ambiguous, such as
    * `_Alignas(sizeof(T))`, and the whole file is lost. The graph does not represent alignment, so it is defined away.
    */
  val ALIGNMENT_SPECIFIER: Map[String, String] = Map("_Alignas(x)" -> "")

  /** C11's `_Thread_local`, which CDT's C parser does not know. Without this define, `static _Thread_local int x;` at
    * file scope is a problem declaration and `x` is lost. It is defined as `__thread`, GCC's keyword for the same
    * storage class, which CDT's GNU configuration defines away.
    */
  private[parser] val C11_KEYWORDS: Map[String, String] = Map("_Thread_local" -> "__thread")
}
