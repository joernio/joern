package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.joern.x2cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class CallTests extends SwiftCompilerSrc2CpgSuite {

  "CallTests" should {

    "be correct for simple calls" in {
      val testCode =
        """
          |class Foo {
          |  func foo() {}
          |  func bar() {}
          |  func main() {
          |    foo()
          |    self.bar()
          |    other.method()
          |  }
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      fooCall.signature shouldBe ""
      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      barCall.signature shouldBe ""
      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

      val List(methodCall) = cpg.call.nameExact("method").l
      methodCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      methodCall.signature shouldBe ""
      val List(methodCallReceiverCall) = methodCall.receiver.isIdentifier.l
      methodCallReceiverCall.name shouldBe "other"
      methodCallReceiverCall.typeFullName shouldBe "ANY"
    }

    "be correct for simple calls with compiler support" in {
      val testCode =
        """
          |class Foo {
          |  func foo() {}
          |  func bar() -> String { return "" }
          |  func main() {
          |    foo()
          |    self.bar()
          |  }
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe "SwiftTest.Foo.foo:()->()"
      fooCall.signature shouldBe "()->()"
      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "SwiftTest.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe "SwiftTest.Foo.bar:()->Swift.String"
      barCall.signature shouldBe "()->Swift.String"
      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "SwiftTest.Foo"
    }

  }

}
