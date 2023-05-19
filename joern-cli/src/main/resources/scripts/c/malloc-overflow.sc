@main def main(): List[Call] = {
  cpg
    .call("malloc")
    .filter { mallocCall =>
      mallocCall.argument(1) match {
        case subCall: Call =>
          subCall.name == Operators.addition || subCall.name == Operators.multiplication
        case _ => false
      }
    }
    .l
}
