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

    "be correct for call to objc constructor with compiler support" in {
      val testCode =
        """
          |import Foundation
          |
          |class Foo : NSObject {}
          |
          |func main() {
          |  var x = Foo()
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

      cpg.identifier.nameExact("x").typeFullName.loneElement shouldBe "SwiftTest.Foo"
    }

    "be correct for call to obj constructor with parameters with compiler support" in {
      val testCode =
        """
          |import Foundation
          |
          |class Foo : NSObject {
          |  init(x: Int, y: String) {}
          |}
          |
          |func main() {
          |  var x = Foo(x: 1, y: "a")
          |}
          |""".stripMargin

      val cpg = codeWithSwiftSetup(testCode)

      val List(constructorCallBlock) = cpg.block.codeExact("Foo(x: 1, y: \"a\")").l
      val List(tmpAssignment)        = constructorCallBlock.astChildren.isCall.isAssignment.l
      tmpAssignment.code shouldBe s"<tmp>0 = ${Operators.alloc}"
      tmpAssignment.argument.isIdentifier.typeFullName.l shouldBe List("SwiftTest.Foo")
      tmpAssignment.argument.isCall.name.l shouldBe List(Operators.alloc)

      val List(constructorCall) = constructorCallBlock.astChildren.isCall.nameExact("init").l
      constructorCall.typeFullName shouldBe "SwiftTest.Foo"
      constructorCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      constructorCall.methodFullName shouldBe "SwiftTest.Foo.init:(x:Swift.Int,y:Swift.String)->SwiftTest.Foo"
      constructorCall.typeFullName shouldBe "SwiftTest.Foo"
      constructorCall.signature shouldBe "(x:Swift.Int,y:Swift.String)->SwiftTest.Foo"
      constructorCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      constructorCall.argument.size shouldBe 3
      constructorCall.arguments(0).isIdentifier.name.loneElement shouldBe "<tmp>0"
      constructorCall.arguments(0).isIdentifier.typeFullName.loneElement shouldBe "SwiftTest.Foo"
      constructorCall.arguments(1).isLiteral.code.loneElement shouldBe "1"
      constructorCall.arguments(2).isLiteral.code.loneElement shouldBe "\"a\""

      val List(returnId) = constructorCallBlock.astChildren.isIdentifier.nameExact("<tmp>0").l
      returnId.typeFullName shouldBe "SwiftTest.Foo"

      cpg.typeDecl
        .fullNameExact("SwiftTest.Foo")
        .method
        .isConstructor
        .fullName
        .loneElement shouldBe constructorCall.methodFullName
      cpg.identifier.nameExact("x").typeFullName.loneElement shouldBe "SwiftTest.Foo"
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

    "be correct for static member access" in {
      val testCode = """
       |class Foo {
       |    static let aaa: Int = Foo.source()
       |
       |    static func source() -> Int {
       |        return 1
       |    }
       |    
       |    func foo() {
       |        print(Foo.aaa)
       |    }
       |}
       |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(aaaAccess) = cpg.fieldAccess.codeExact("Foo.aaa").l
      val List(fooRef)    = aaaAccess.arguments(1).isTypeRef.l
      fooRef.typeFullName shouldBe "SwiftTest.Foo"

      val List(sourceCall) = cpg.call.nameExact("source").l
      sourceCall.methodFullName shouldBe "SwiftTest.Foo.source:()->Swift.Int"
      sourceCall.signature shouldBe "()->Swift.Int"
      sourceCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(sourceMethod) = cpg.method.nameExact("source").l
      sourceMethod.fullName shouldBe "SwiftTest.Foo.source:()->Swift.Int"
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

    "use compiler information to distinguish static from instance calls on uppercase bases with compiler support" in {
      val testCode =
        """
          |class Foo {
          |  nonisolated(unsafe) static let shared = Foo()
          |  static func staticFunc() {}
          |  class func classFunc() {}
          |  func instFunc() {}
          |}
          |func main() {
          |  Foo.staticFunc()
          |  Foo.classFunc()
          |  Foo.shared.instFunc()
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(staticFuncCall) = cpg.call.nameExact("staticFunc").l
      staticFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      staticFuncCall.methodFullName shouldBe "SwiftTest.Foo.staticFunc:()->()"
      staticFuncCall.signature shouldBe "()->()"

      val List(classFuncCall) = cpg.call.nameExact("classFunc").l
      classFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      classFuncCall.methodFullName shouldBe "SwiftTest.Foo.classFunc:()->()"
      classFuncCall.signature shouldBe "()->()"

      val List(instFuncCall) = cpg.call.nameExact("instFunc").l
      instFuncCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      instFuncCall.methodFullName shouldBe "SwiftTest.Foo.instFunc:()->()"
      instFuncCall.signature shouldBe "()->()"
    }

    "use compiler information for static calls on structs, enums, protocols and Self with compiler support" in {
      val testCode =
        """
          |struct S {
          |  static func sFunc() {}
          |  func sInst() {}
          |  static func viaSelf() { Self.sFunc() }
          |}
          |enum E {
          |  case a
          |  case withPayload(Int)
          |  static func eFunc() {}
          |}
          |struct Outer {
          |  struct Inner {}
          |}
          |protocol P {
          |  static func pFunc()
          |}
          |extension P {
          |  static func pExt() {}
          |}
          |struct Q: P {
          |  static func pFunc() {}
          |}
          |func main() {
          |  S.sFunc()
          |  S().sInst()
          |  E.eFunc()
          |  let payload = E.withPayload(1)
          |  let inner = Outer.Inner()
          |  Q.pFunc()
          |  Q.pExt()
          |}
          |""".stripMargin
      val cpg = codeWithSwiftSetup(testCode)

      val List(sFuncCall) = cpg.call.codeExact("S.sFunc()").l
      sFuncCall.methodFullName shouldBe "SwiftTest.S.sFunc:()->()"
      sFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(selfFuncCall) = cpg.call.codeExact("Self.sFunc()").l
      selfFuncCall.methodFullName shouldBe "SwiftTest.S.sFunc:()->()"
      selfFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(sInstCall) = cpg.call.nameExact("sInst").l
      sInstCall.methodFullName shouldBe "SwiftTest.S.sInst:()->()"
      sInstCall.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(eFuncCall) = cpg.call.nameExact("eFunc").l
      eFuncCall.methodFullName shouldBe "SwiftTest.E.eFunc:()->()"
      eFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(withPayloadCall) = cpg.call.nameExact("withPayload").l
      withPayloadCall.methodFullName shouldBe "SwiftTest.E.withPayload:(SwiftTest.E.Type)->(Swift.Int)->SwiftTest.E"
      withPayloadCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(innerCall) = cpg.call.codeExact("Outer.Inner()").l
      innerCall.methodFullName shouldBe "SwiftTest.Outer.Inner.init:()->SwiftTest.Outer.Inner"
      innerCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(pFuncCall) = cpg.call.nameExact("pFunc").l
      pFuncCall.methodFullName shouldBe "SwiftTest.Q.pFunc:()->()"
      pFuncCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(pExtCall) = cpg.call.nameExact("pExt").l
      pExtCall.methodFullName shouldBe "SwiftTest.P<extension>.pExt:()->()"
      pExtCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "be correct for implicit member expressions in call arguments" in {
      val testCode =
        """
          |enum Color {
          |  case red
          |  case blue
          |}
          |
          |func takesColor(_ c: Color) {}
          |
          |func main() {
          |  takesColor(.red)
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(takesColorCall) = cpg.call.nameExact("takesColor").l
      val implicitMemberArg    = takesColorCall.arguments(1).loneElement.asInstanceOf[Unknown]
      implicitMemberArg.code shouldBe "red"

      cpg.fieldAccess.l shouldBe empty
    }

    "be correct for implicit member expressions in assignments" in {
      val testCode =
        """
          |enum Color {
          |  case red
          |}
          |
          |func main() {
          |  let c: Color = .red
          |}
          |""".stripMargin
      val cpg = code(testCode)

      val List(assignCall) = cpg.call.nameExact(Operators.assignment).l
      val rhs              = assignCall.arguments(2).loneElement.asInstanceOf[Unknown]
      rhs.code shouldBe "red"

      cpg.fieldAccess.l shouldBe empty
    }

  }

}
