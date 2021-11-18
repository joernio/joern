import io.shiftleft.semanticcpg.language._

@main def main(pathToCode: String, minMethodCount: Int, expectedMethod: String) = {
  importCode(pathToCode)
  val methodCount = cpg.method.size
  assert(methodCount >= minMethodCount,
         s"expected at least $minMethodCount methods, but only found $methodCount")

  val methodNames = cpg.method.name.toSet
  assert(methodNames.contains(expectedMethod), s"expected method `$expectedMethod` not found... available methods: $methodNames")
}

