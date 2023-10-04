package io.joern.gosrc2cpg.astcreation

object Defines {
  val anyTypeName: String            = "ANY"
  val voidTypeName: String           = "void"
  val qualifiedNameSeparator: String = "::"
  val empty                          = "<empty>"
  val dot                            = "."
  val map                            = "map"
  val chan                           = "chan"
  val This: String                   = "this"
  val Bool                           = "bool"
  val FieldAccess                    = "<FieldAccess>"
  val ReturnType                     = "<ReturnType>"

  val primitiveTypeMap: Map[String, String] =
    // This list is prepared with reference to primitives defined at https://pkg.go.dev/builtin#pkg-types
    Map(
      ("any", "any"),
      ("interface{}", "any"),
      ("bool", "bool"),
      ("byte", "uint8"),
      ("comparable", "comparable"),
      ("complex128", "complex128"),
      ("complex64", "complex64"),
      ("error", "error"),
      ("float32", "float32"),
      ("float64", "float64"),
      ("int", "int"),
      ("int8", "int8"),
      ("int16", "int16"),
      ("int32", "int32"),
      ("int64", "int64"),
      ("rune", "int32"),
      ("string", "string"),
      ("uint", "uint"),
      ("uint8", "uint8"),
      ("uint16", "uint16"),
      ("uint32", "uint32"),
      ("uint64", "uint64"),
      ("uintptr", "uintptr")
    )
  val builtinFunctions: Map[String, (String, String, String)] =
    // Prepared referring to this - https://pkg.go.dev/builtin#pkg-functions
    Map(
      ("append", ("append([]any, []any)[]any", "append", "[]any")),
      ("cap", ("cap(any)int", "cap", "int")),
      ("clear", ("clear[[]any|map[any]any](any)", "clear", voidTypeName)),
      ("close", ("close(chan<-any)", "close", voidTypeName)),
      ("complex", ("complex(FloatType)ComplexType", "complex", "ComplexType")),
      ("copy", ("copy([]any, []any)int", "copy", "int")),
      ("delete", ("delete(map[any]any, any)", "delete", voidTypeName)),
      ("imag", ("imag(ComplexType)FloatType", "imag", "FloatType")),
      ("len", ("len(any)int", "len", "int")),
      ("make", ("make(map)map|make(chan)chan", "make", "map|chan")),
      ("max", ("max[cmp.Ordered](cmp.Ordered, []cmp.Ordered)cmp.Ordered", "max", "cmp.Ordered")),
      ("min", ("min[cmp.Ordered](cmp.Ordered, []cmp.Ordered)cmp.Ordered", "min", "cmp.Ordered")),
      ("new", ("new(any)*any", "new", "*any")),
      ("panic", ("panic(any)", "panic", voidTypeName)),
      ("print", ("print([]any)", "print", voidTypeName)),
      ("println", ("println([]any)", "println", voidTypeName)),
      ("real", ("real(ComplexType)", "real", voidTypeName)),
      ("recover", ("recover()any", "recover", "any"))
    )
}
