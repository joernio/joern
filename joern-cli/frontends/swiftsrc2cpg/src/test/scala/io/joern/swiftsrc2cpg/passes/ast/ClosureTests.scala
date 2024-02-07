package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ClosureTests extends AbstractPassTest {

  "ClosureTests" should {

    "testClosure1" in AstFixture("""
        |reversedNames = names.sorted(by: { (s1: String, s2: String) -> Bool in return s1 > s2 } )
        |""".stripMargin) { cpg =>
      val List(closureRef) = cpg.call("sorted").argument(1).isCall.argument.isMethodRef.l
      closureRef.methodFullName shouldBe "code.swift:<global>:<lambda>0"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "code.swift:<global>:<lambda>0"
      closureMethod.code shouldBe "{ (s1: String, s2: String) -> Bool in return s1 > s2 }"
      closureMethod.signature shouldBe "Bool code.swift:<global>:<lambda>0 (s1: String, s2: String)"
      val List(p1, p2) = closureMethod.parameter.l
      p1.code shouldBe "s1: String"
      p1.name shouldBe "s1"
      p1.typeFullName shouldBe "String"
      p2.code shouldBe "s2: String"
      p2.name shouldBe "s2"
      p2.typeFullName shouldBe "String"
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("return s1 > s2")
    }

    "testClosure2" in AstFixture("""
        |// implicit return
        |reversedNames = names.sorted(by: { s1, s2 in s1 > s2 } )
        |""".stripMargin) { cpg =>
      val List(closureRef) = cpg.call("sorted").argument(1).isCall.argument.isMethodRef.l
      closureRef.methodFullName shouldBe "code.swift:<global>:<lambda>0"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "code.swift:<global>:<lambda>0"
      closureMethod.code shouldBe "{ s1, s2 in s1 > s2 }"
      closureMethod.signature shouldBe "ANY code.swift:<global>:<lambda>0 (s1, s2)"
      val List(p1, p2) = closureMethod.parameter.l
      p1.code shouldBe "s1"
      p1.name shouldBe "s1"
      p1.typeFullName shouldBe "ANY"
      p2.code shouldBe "s2"
      p2.name shouldBe "s2"
      p2.typeFullName shouldBe "ANY"
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("s1 > s2")
    }

    "testClosure3" in AstFixture("""
        |someFunctionThatTakesAClosure(closure: { foo() })
        |""".stripMargin) { cpg =>
      val List(call)       = cpg.call.nameExact("someFunctionThatTakesAClosure").l
      val List(closureRef) = call.argument.isCall.argument(2).isMethodRef.l
      closureRef.methodFullName shouldBe "code.swift:<global>:<lambda>0"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "code.swift:<global>:<lambda>0"
      closureMethod.code shouldBe "{ foo() }"
      closureMethod.signature shouldBe "ANY code.swift:<global>:<lambda>0 ()"
      closureMethod.parameter.size shouldBe 0
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("foo()")
    }

    "testClosure4" in AstFixture("""
        |someFunctionThatTakesAClosure() { foo() }
        |""".stripMargin) { cpg =>
      val List(call)       = cpg.call.nameExact("someFunctionThatTakesAClosure").l
      val List(closureRef) = call.argument.isMethodRef.l
      closureRef.methodFullName shouldBe "code.swift:<global>:<lambda>0"
      val List(closureMethod) = cpg.method.nameExact("<lambda>0").l
      closureMethod.fullName shouldBe "code.swift:<global>:<lambda>0"
      closureMethod.code shouldBe "{ foo() }"
      closureMethod.signature shouldBe "ANY code.swift:<global>:<lambda>0 ()"
      closureMethod.parameter.size shouldBe 0
      closureMethod.block.astChildren.isReturn.code.l shouldBe List("foo()")
    }

    "testClosure5" in AstFixture("""
        |var foo = { (_ x: MyType) in }
        |var bar = { (x y: MyType) in }
        |""".stripMargin) { cpg =>
      val List(fooAssignment, barAssignment) = cpg.call.l
      val closure1                           = fooAssignment.argument(2).asInstanceOf[MethodRef]
      closure1.methodFullName shouldBe "code.swift:<global>:<lambda>0"
      val closure2 = barAssignment.argument(2).asInstanceOf[MethodRef]
      closure2.methodFullName shouldBe "code.swift:<global>:<lambda>1"
      val List(closureMethod1) = cpg.method.nameExact("<lambda>0").l
      closureMethod1.fullName shouldBe "code.swift:<global>:<lambda>0"
      closureMethod1.code shouldBe "{ (_ x: MyType) in }"
      closureMethod1.signature shouldBe "ANY code.swift:<global>:<lambda>0 (_ x: MyType)"
      val List(p1) = closureMethod1.parameter.l
      p1.code shouldBe "_ x: MyType"
      p1.name shouldBe "x"
      p1.typeFullName shouldBe "MyType"
      val List(closureMethod2) = cpg.method.nameExact("<lambda>1").l
      closureMethod2.fullName shouldBe "code.swift:<global>:<lambda>1"
      closureMethod2.code shouldBe "{ (x y: MyType) in }"
      closureMethod2.signature shouldBe "ANY code.swift:<global>:<lambda>1 (x y: MyType)"
      val List(p2) = closureMethod2.parameter.l
      p2.code shouldBe "x y: MyType"
      p2.name shouldBe "y"
      p2.typeFullName shouldBe "MyType"
    }

  }

}
