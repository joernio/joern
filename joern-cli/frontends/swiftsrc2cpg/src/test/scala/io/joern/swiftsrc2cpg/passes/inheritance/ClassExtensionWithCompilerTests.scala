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
          |extension Foo : SomeProtocol, AnotherProtocol {
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
      ).moreCode(
        """
          |protocol SomeProtocol {}
          |""".stripMargin,
        "SwiftTest/Sources/SomeProtocol.swift"
      ).moreCode(
        """
          |protocol AnotherProtocol {}
          |""".stripMargin,
        "SwiftTest/Sources/AnotherProtocol.swift"
      )
      cpg.call.codeExact("print(a)").argument.isCall.code.l shouldBe List("self.a")
      cpg.call.codeExact("print(a)").argument.isCall.typeFullName.l shouldBe List("Swift.String")
      cpg.call.codeExact("print(a)").argument.isCall.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")

      cpg.call.codeExact("print(b)").argument.isIdentifier.typeFullName.l shouldBe List("Swift.String")

      cpg.call.codeExact("print(c)").argument.isCall.code.l shouldBe List("self.c")
      cpg.call.codeExact("print(c)").argument.isCall.typeFullName.l shouldBe List("Swift.String")
      cpg.call.codeExact("print(c)").argument.isCall.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")

      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.inheritsFromTypeFullName.l should contain allElementsOf List(
        "SwiftTest.AnotherProtocol",
        "SwiftTest.SomeProtocol"
      )

      /** TODO: Re-enable once extension methods are properly accessible via EXTENSION_BLOCK foo.boundMethod.fullName.l
        * shouldBe List( "SwiftTest.Foo.init:()->SwiftTest.Foo", "SwiftTest.Foo<extension>.foo:()->()" )
        */

      cpg.typ.name.l.distinct shouldBe cpg.typ.name.l
      cpg.typ.nameExact("AnotherProtocol")._typeDeclViaInheritsFromIn.fullName.l shouldBe List("SwiftTest.Foo")
      cpg.typ.nameExact("SomeProtocol")._typeDeclViaInheritsFromIn.fullName.l shouldBe List("SwiftTest.Foo")
    }

  }

}
