package io.joern.gosrc2cpg.astcreation

object Defines {
  val anyTypeName: String            = "ANY"
  val voidTypeName: String           = "void"
  val qualifiedNameSeparator: String = "::"
  val empty                          = "<empty>"

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
}
