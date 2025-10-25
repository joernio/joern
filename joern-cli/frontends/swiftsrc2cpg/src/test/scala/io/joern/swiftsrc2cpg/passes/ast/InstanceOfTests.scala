package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class InstanceOfTests extends SwiftCompilerSrc2CpgSuite {

  "InstanceOfTests" should {
    "have correct structure for instanceOf checks" in {
      val cpg = code("""
          |func main(x: Int) -> Int {
          |  if x is [[String: Any]] {
          |    return 2
          |  }
          |  return 1
          |}""".stripMargin)
      inside(cpg.call.nameExact(Operators.instanceOf).argument.l) { case List(x: Identifier, typeRef: TypeRef) =>
        x.typeFullName shouldBe Defines.Int
        typeRef.typeFullName shouldBe "Swift.Array"
      }
    }

    "have correct structure for instanceOf checks with compiler support" in {
      val cpg = codeWithSwiftSetup("""
          |func main(x: Int) -> Int {
          |  if x is [[String: Any]] {
          |    return 2
          |  }
          |  return 1
          |}""".stripMargin)
      inside(cpg.call.nameExact(Operators.instanceOf).argument.l) { case List(x: Identifier, typeRef: TypeRef) =>
        x.typeFullName shouldBe Defines.Int
        // Sadly, swiftc does not offer any information about the actual (deep) type of the RSH of the check
        typeRef.typeFullName shouldBe "Swift.Array"
      }
    }
  }

}
