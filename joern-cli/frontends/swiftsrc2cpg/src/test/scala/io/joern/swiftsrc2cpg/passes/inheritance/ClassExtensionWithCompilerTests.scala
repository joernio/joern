package io.joern.swiftsrc2cpg.passes.inheritance

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ClassExtensionWithCompilerTests extends SwiftCompilerSrc2CpgSuite {

  "ClassExtensionWithCompilerTests" should {

    "resolve self correctly (separate files)" in {
      val cpg = codeWithSwiftSetup(
        s"""
           |class Foo {
           |  var a = "a"
           |  var b = 1
           |  var c = "c"
           |}""".stripMargin,
        "main.swift"
      ).moreCode(
        s"""
          |extension Foo {
          |  // var c = "c" --> compiler says: extensions must not contain stored properties
          |
          |  func foo() {
          |    print(a)
          |    var b = "b"
          |    print(b)
          |    print(c)
          |  }
          |}""".stripMargin,
        "SwiftTest/Sources/FooExt.swift"
      )
      cpg.call.codeExact("print(a)").argument.isCall.code.l shouldBe List("self.a")
      cpg.call.codeExact("print(a)").argument.isCall.typeFullName.l shouldBe List("Swift.String")
      cpg.call.codeExact("print(a)").argument.isCall.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")

      cpg.call.codeExact("print(b)").argument.isIdentifier.typeFullName.l shouldBe List("Swift.String")

      cpg.call.codeExact("print(c)").argument.isCall.code.l shouldBe List("self.c")
      cpg.call.codeExact("print(c)").argument.isCall.typeFullName.l shouldBe List("Swift.String")
      cpg.call.codeExact("print(c)").argument.isCall.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")
    }

  }

}
