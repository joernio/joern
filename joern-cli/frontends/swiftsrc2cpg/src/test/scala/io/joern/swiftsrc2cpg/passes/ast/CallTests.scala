package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftCompilerSrc2CpgSuite
import io.joern.x2cpg
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
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
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      barCall.signature shouldBe ""
      barCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

      val List(methodCall) = cpg.call.nameExact("method").l
      methodCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      methodCall.signature shouldBe ""
      methodCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

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
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "SwiftTest.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe "SwiftTest.Foo.bar:()->Swift.String"
      barCall.signature shouldBe "()->Swift.String"
      barCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "SwiftTest.Foo"
    }

    "be correct for simple call to constructor with compiler support" in {
      val testCode =
        """
          |class Foo {}
          |
          |func main() {
          |  Foo()
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(constructorCallBlock) = cpg.block.codeExact("Foo()").l
      val List(tmpAssignment)        = constructorCallBlock.astChildren.isCall.isAssignment.l
      tmpAssignment.code shouldBe s"<tmp>0 = ${Operators.alloc}"
      tmpAssignment.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")
      tmpAssignment.argument.isCall.name.l shouldBe List(Operators.alloc)
      val List(constructorCall) = constructorCallBlock.astChildren.isCall.nameExact("init").l
      constructorCall.methodFullName shouldBe "SwiftTest.Foo.init:()->SwiftTest.Foo"
      constructorCall.typeFullName shouldBe "SwiftTest.Foo"
      constructorCall.signature shouldBe "()->SwiftTest.Foo"
      constructorCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      constructorCall.argument.isIdentifier.name.l shouldBe List("<tmp>0")
      constructorCall.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")
      val List(returnId) = constructorCallBlock.astChildren.isIdentifier.nameExact("<tmp>0").l
      returnId.typeFullName shouldBe "SwiftTest.Foo"
    }

    "be correct for simple calls to functions from extensions" in {
      val testCode =
        """
          |extension Foo {
          |  func foo() {}
          |  func bar() {}
          |}
          |class Foo {
          |  func main() {
          |    foo()
          |    self.bar()
          |  }
          |}
          |""".stripMargin
      pendingUntilFixed {
        val cpg = code(testCode)

        // These extension calls should be static calls.
        // Currently, there is no way to detect this as we have no correct method fullnames at calls without compiler support at all.
        val List(fooCall) = cpg.call.nameExact("foo").l
        fooCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
        fooCall.signature shouldBe ""
        fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH // Should be STATIC_DISPATCH for extension methods, but it is not

        val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
        fooCallReceiver.name shouldBe "self"
        fooCallReceiver.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

        val List(barCall) = cpg.call.nameExact("bar").l
        barCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
        barCall.signature shouldBe ""
        barCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH // Should be STATIC_DISPATCH for extension methods, but it is not

        val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
        barCallReceiverCall.name shouldBe "self"
        barCallReceiverCall.typeFullName shouldBe "Sources/main.swift:<global>.Foo"
      }
    }

    "be correct for simple calls to functions from extensions with compiler support" in {
      val testCode =
        """
          |extension Foo {
          |  func foo() {}
          |  func bar() {}
          |}
          |class Foo {
          |  func main() {
          |    foo()
          |    self.bar()
          |  }
          |}
          |""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)

      /** TODO: Re-enable once extension methods are properly accessible via EXTENSION_BLOCK
        * cpg.typeDecl.nameExact("Foo").boundMethod.fullName.l shouldBe List( "SwiftTest.Foo.init:()->SwiftTest.Foo",
        * "SwiftTest.Foo.main:()->()", "SwiftTest.Foo<extension>.foo:()->()", "SwiftTest.Foo<extension>.bar:()->()" )
        */
      val List(fooMethod) = cpg.method.nameExact("foo").l
      fooMethod.fullName shouldBe "SwiftTest.Foo<extension>.foo:()->()"
      val List(barMethod) = cpg.method.nameExact("bar").l
      barMethod.fullName shouldBe "SwiftTest.Foo<extension>.bar:()->()"

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe fooMethod.fullName
      fooCall.signature shouldBe "()->()"
      fooCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      fooCall.receiver shouldBe empty
      val List(fooCallBase) = fooCall.arguments(0).isIdentifier.l
      fooCallBase.name shouldBe "self"
      fooCallBase.typeFullName shouldBe "SwiftTest.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe barMethod.fullName
      barCall.signature shouldBe "()->()"
      barCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      barCall.receiver shouldBe empty
      val List(barCallBase) = barCall.arguments(0).isIdentifier.l
      barCallBase.name shouldBe "self"
      barCallBase.typeFullName shouldBe "SwiftTest.Foo"
    }

    "be correct for simple calls to extension from library" in {
      val testCode =
        """
          |var x = 1
          |x.negate()
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(negateCall) = cpg.call.nameExact("negate").l
      negateCall.methodFullName shouldBe "Swift.SignedNumeric<extension>.negate:()->()"
    }

    "be correct for calls to generic functions from extensions with compiler support" in {
      val testCode =
        """
          |class Foo<T> {}
          |
          |extension Foo<Int> {
          |  func bar() {}
          |}
          |
          |extension Foo<Double> {
          |  func bar() {}
          |}
          |
          |func main() {
          |  Foo<Int>().bar()
          |  Foo<Double>().bar()
          |}
          |""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)

      val List(barMethodInt, barMethodDouble) = cpg.method.nameExact("bar").l
      barMethodInt.fullName shouldBe "SwiftTest.Foo<AwhereA==Swift.Int><extension>.bar:()->()"
      barMethodDouble.fullName shouldBe "SwiftTest.Foo<AwhereA==Swift.Double><extension>.bar:()->()"

      val List(intBarCall, doubleBarCall) = cpg.call.nameExact("bar").l
      intBarCall.methodFullName shouldBe barMethodInt.fullName
      intBarCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      intBarCall.receiver shouldBe empty
      val List(base) = intBarCall.arguments(0).l
      base.code shouldBe "Foo<Int>()"

      doubleBarCall.methodFullName shouldBe barMethodDouble.fullName
      doubleBarCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "be correct for simple calls to functions from protocols" in {
      val testCode =
        """
          |protocol FooProtocol {
          |  func foo()
          |  func bar()
          |}
          |class Foo: FooProtocol {
          |  func foo() {}
          |  func bar() {}
          |  func main() {
          |    foo()
          |    self.bar()
          |  }
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      fooCall.signature shouldBe ""
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "Sources/main.swift:<global>.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe x2cpg.Defines.DynamicCallUnknownFullName
      barCall.signature shouldBe ""
      barCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "Sources/main.swift:<global>.Foo"
    }

    "be correct for simple calls to functions from protocols with compiler support" in {
      val testCode =
        """
          |protocol FooProtocol {
          |  func foo()
          |  func bar()
          |}
          |class Foo: FooProtocol {
          |  func foo() {}
          |  func bar() {}
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
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "SwiftTest.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe "SwiftTest.Foo.bar:()->()"
      barCall.signature shouldBe "()->()"
      barCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "SwiftTest.Foo"
    }

    "be correct for simple calls to functions from multiple protocols with compiler support" in {
      val testCode =
        """
          |protocol FooProtocol {
          |  func foo()
          |  func bar()
          |}
          |protocol FooBarProtocol {
          |  func foobar()
          |}
          |class Foo: FooProtocol, FooBarProtocol {
          |  func foo() {}
          |  func bar() {}
          |  func foobar() {}
          |  func main() {
          |    foo()
          |    self.bar()
          |    foobar()
          |  }
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.methodFullName shouldBe "SwiftTest.Foo.foo:()->()"
      fooCall.signature shouldBe "()->()"
      fooCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      val List(fooCallReceiver) = fooCall.receiver.isIdentifier.l
      fooCallReceiver.name shouldBe "self"
      fooCallReceiver.typeFullName shouldBe "SwiftTest.Foo"

      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.methodFullName shouldBe "SwiftTest.Foo.bar:()->()"
      barCall.signature shouldBe "()->()"
      barCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(barCallReceiverCall) = barCall.receiver.isIdentifier.l
      barCallReceiverCall.name shouldBe "self"
      barCallReceiverCall.typeFullName shouldBe "SwiftTest.Foo"

      val List(foobarCall) = cpg.call.nameExact("foobar").l
      foobarCall.methodFullName shouldBe "SwiftTest.Foo.foobar:()->()"
      foobarCall.signature shouldBe "()->()"
      foobarCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(foobarCallReceiverCall) = foobarCall.receiver.isIdentifier.l
      foobarCallReceiverCall.name shouldBe "self"
      foobarCallReceiverCall.typeFullName shouldBe "SwiftTest.Foo"
    }

    "be correct for simple call to static function" in {
      // TODO: extend the GsonTypeInfoReader to query for information whether the call is a call to a static function
      val testCode =
        """
          |func main() {
          |  Foo.staticFunc()
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(staticFuncCall) = cpg.call.nameExact("staticFunc").l
      staticFuncCall.methodFullName shouldBe "Foo.staticFunc"
      staticFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      staticFuncCall.argument shouldBe empty
    }

  }

}
