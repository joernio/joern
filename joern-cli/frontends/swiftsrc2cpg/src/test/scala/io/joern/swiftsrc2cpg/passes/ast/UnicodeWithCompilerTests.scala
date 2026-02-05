package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class UnicodeWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "UnicodeWithCompilerTests" should {

    "be correct with unicode embedded" in {
      val cpg = codeWithSwiftSetup("""
         |struct Factory {
         |  var unicode: String = "✓ Hello, World! ✓"
         |}
         |
         |extension Factory {
         |  static func id(x: Int) -> Int {
         |    print("Hello, World!")
         |    return x
         |  }
         |}
         |func main(source: Int) {
         |  // ✓ This works
         |  Factory.id(x: source)
         |}
         |""".stripMargin)
      val List(idCall) = cpg.call("id").l
      idCall.code shouldBe "Factory.id(x: source)"
      idCall.methodFullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"
      val List(idMethod) = cpg.method("id").l
      idMethod.fullName shouldBe "SwiftTest.Factory<extension>.id:(x:Swift.Int)->Swift.Int"
    }

  }

}
