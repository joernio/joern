//> using file assertions.sc

@main def main(inputPath: String) = {
  importCode(inputPath)
  val calls = cpg
    .call("malloc")
    .filter { mallocCall =>
      mallocCall.argument(1) match {
        case subCall: nodes.Call =>
          subCall.name == Operators.addition || subCall.name == Operators.multiplication
        case _ => false
      }
    }
    .code

  val expected = Set("malloc(sizeof(int) * 42)", "malloc(sizeof(int) * 3)", "malloc(sizeof(int) + 55)")
  assertContains("calls", calls, expected)
}
