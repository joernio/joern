package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ClosureTests extends SwiftSrc2CpgSuite {

  "ClosureTests" should {

    "testClosure1" in {
      val cpg = code("""
        |reversedNames = names.sorted(by: { (s1: String, s2: String) -> Bool in return s1 > s2 } )
        |""".stripMargin)
      val List(closureRef) = cpg.call("sorted").argument.isMethodRef.l
      closureRef.methodFullName shouldBe "Test0.swift:<global>.<lambda>0:(s1:Swift.String,s2:Swift.String)->Swift.Bool"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "Test0.swift:<global>.<lambda>0:(s1:Swift.String,s2:Swift.String)->Swift.Bool"
      closureMethod.code shouldBe "{ (s1: String, s2: String) -> Bool in return s1 > s2 }"
      closureMethod.signature shouldBe "(s1:Swift.String,s2:Swift.String)->Swift.Bool"
      val List(p1, p2) = closureMethod.parameter.l
      p1.code shouldBe "s1: String"
      p1.name shouldBe "s1"
      p1.typeFullName shouldBe "Swift.String"
      p2.code shouldBe "s2: String"
      p2.name shouldBe "s2"
      p2.typeFullName shouldBe "Swift.String"
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("return s1 > s2")
    }

    "testClosure2" in {
      val cpg = code("""
        |// implicit return
        |reversedNames = names.sorted(by: { s1, s2 in s1 > s2 } )
        |""".stripMargin)
      val List(closureRef) = cpg.call("sorted").argument.isMethodRef.l
      closureRef.methodFullName shouldBe "Test0.swift:<global>.<lambda>0:(ANY,ANY)->ANY"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "Test0.swift:<global>.<lambda>0:(ANY,ANY)->ANY"
      closureMethod.code shouldBe "{ s1, s2 in s1 > s2 }"
      closureMethod.signature shouldBe "(ANY,ANY)->ANY"
      val List(p1, p2) = closureMethod.parameter.l
      p1.code shouldBe "s1"
      p1.name shouldBe "s1"
      p1.typeFullName shouldBe "ANY"
      p2.code shouldBe "s2"
      p2.name shouldBe "s2"
      p2.typeFullName shouldBe "ANY"
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("s1 > s2")
    }

    "testClosure3" in {
      val cpg = code("""
        |someFunctionThatTakesAClosure(closure: { foo() })
        |""".stripMargin)
      val List(call)       = cpg.call.nameExact("someFunctionThatTakesAClosure").l
      val List(closureRef) = call.argument.isMethodRef.l
      closureRef.methodFullName shouldBe "Test0.swift:<global>.<lambda>0:()->ANY"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "Test0.swift:<global>.<lambda>0:()->ANY"
      closureMethod.code shouldBe "{ foo() }"
      closureMethod.signature shouldBe "()->ANY"
      closureMethod.parameter.size shouldBe 0
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("foo()")
    }

    "testClosure4" in {
      val cpg = code("""
        |someFunctionThatTakesAClosure() { foo() }
        |""".stripMargin)
      val List(call)       = cpg.call.nameExact("someFunctionThatTakesAClosure").l
      val List(closureRef) = call.argument.isMethodRef.l
      closureRef.methodFullName shouldBe "Test0.swift:<global>.<lambda>0:()->ANY"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "Test0.swift:<global>.<lambda>0:()->ANY"
      closureMethod.code shouldBe "{ foo() }"
      closureMethod.signature shouldBe "()->ANY"
      closureMethod.parameter.size shouldBe 0
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("foo()")
    }

    "testClosure5" in {
      val cpg = code("""
        |var foo = { (_ x: MyType) in }
        |var bar = { (x y: MyType) in }
        |""".stripMargin)
      val List(fooAssignment, barAssignment) = cpg.call.l
      val closure1Ref                        = fooAssignment.argument(2).asInstanceOf[MethodRef]
      closure1Ref.methodFullName shouldBe "Test0.swift:<global>.<lambda>0:(_:MyType)->ANY"
      val closure2Ref = barAssignment.argument(2).asInstanceOf[MethodRef]
      closure2Ref.methodFullName shouldBe "Test0.swift:<global>.<lambda>1:(x:MyType)->ANY"
      val List(closureMethod1) = cpg.method.nameExact("<lambda>0").l
      closureMethod1.fullName shouldBe "Test0.swift:<global>.<lambda>0:(_:MyType)->ANY"
      closureMethod1.code shouldBe "{ (_ x: MyType) in }"
      closureMethod1.signature shouldBe "(_:MyType)->ANY"
      val List(p1) = closureMethod1.parameter.l
      p1.code shouldBe "_ x: MyType"
      p1.name shouldBe "x"
      p1.typeFullName shouldBe "MyType"
      val List(closureMethod2) = cpg.method.nameExact("<lambda>1").l
      closureMethod2.fullName shouldBe "Test0.swift:<global>.<lambda>1:(x:MyType)->ANY"
      closureMethod2.code shouldBe "{ (x y: MyType) in }"
      closureMethod2.signature shouldBe "(x:MyType)->ANY"
      val List(p2) = closureMethod2.parameter.l
      p2.code shouldBe "x y: MyType"
      p2.name shouldBe "y"
      p2.typeFullName shouldBe "MyType"
    }

    "testClosure6" in {
      val cpg = code("""
          |class A {
          |  var x = 1
          |  func foo(_ a: ANY, _ b: ANY, _ c: ANY, _ d: ANY, _ x: ANY) {}
          |  func method() {
          |    let result = a.closureA { paramA in
          |      b.closureB { paramB in
          |        c.closureC { paramC in
          |          d.closureD { paramD in
          |            foo(paramA, paramB, paramC, paramD, x)
          |          }
          |        }
          |      }
          |    }
          |  }
          |}
          |""".stripMargin)
      val List(closureMethod1) = cpg.method.nameExact("<lambda>0").l
      closureMethod1.fullName shouldBe "Test0.swift:<global>.A.method.<lambda>0:(ANY)->ANY"
      closureMethod1.parameter.code.l shouldBe List("paramA")
      val List(closureMethod2) = cpg.method.nameExact("<lambda>1").l
      closureMethod2.fullName shouldBe "Test0.swift:<global>.A.method.<lambda>0.<lambda>1:(ANY)->ANY"
      closureMethod2.parameter.code.l shouldBe List("paramB")
      val List(closureMethod3) = cpg.method.nameExact("<lambda>2").l
      closureMethod3.fullName shouldBe "Test0.swift:<global>.A.method.<lambda>0.<lambda>1.<lambda>2:(ANY)->ANY"
      closureMethod3.parameter.code.l shouldBe List("paramC")
      val List(closureMethod4) = cpg.method.nameExact("<lambda>3").l
      closureMethod4.fullName shouldBe "Test0.swift:<global>.A.method.<lambda>0.<lambda>1.<lambda>2.<lambda>3:(ANY)->ANY"
      closureMethod4.parameter.code.l shouldBe List("paramD")
      cpg.call.nameExact("foo").argument.code.l shouldBe List("self", "paramA", "paramB", "paramC", "paramD", "self.x")
    }
  }

}
