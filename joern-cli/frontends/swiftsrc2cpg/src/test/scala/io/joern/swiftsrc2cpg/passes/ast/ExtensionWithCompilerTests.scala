package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ExtensionWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "ExtensionWithCompilerTests" should {

    "be correct for static extension methods" in {
      val cpg = codeWithSwiftSetup("""
         |struct Factory {}
         |
         |extension Factory {
         |  static func id(x: Int) -> Int {
         |    return x
         |  }
         |}
         |func main(source: Int) {
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
