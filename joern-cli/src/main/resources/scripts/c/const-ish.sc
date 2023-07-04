//> using file assertions.sc

@main def main(inputPath: String) = {
  importCode(inputPath)
  val methods = cpg.method.internal.filter { method =>
    method.start.assignment.target
      .reachableBy(method.parameter.filter(_.code.contains("const")))
      .nonEmpty
  }.name

  val expected = Set(
    "modify_const_struct_member_cpp_cast",
    "modify_const_struct_member_c_cast",
    "modify_const_struct_cpp_cast",
    "modify_const_struct_c_cast"
  )
  assertContains("methods", methods, expected)

}
