// This test file has been translated from swift/test/Parse/optional.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class OptionalTests extends SwiftSrc2CpgSuite {

  "OptionalTests" should {

    "testOptional3a" in {
      val cpg               = code("var c = a?")
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(localC)      = globalBlock.local.nameExact("c").l
      localC.typeFullName shouldBe "ANY"
      val List(assign) = cpg.call.nameExact(Operators.assignment).l
      assign.code shouldBe "var c = a?"
    }

    "testOptional3b" in {
      val cpg               = code("var d : ()? = a?.foo()")
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(localD)      = globalBlock.local.nameExact("d").l
      localD.typeFullName shouldBe "()"
      val List(fooCall) = cpg.call.nameExact("foo").l
      fooCall.code shouldBe "a?.foo()"
    }

    "testOptional4" in {
      val cpg = code("""
        |var e : (() -> A)?
        |var f = e?()
        |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(localE)      = globalBlock.local.nameExact("e").l
      localE.typeFullName shouldBe "Swift.Function<(()->A)>"
      val List(localF) = globalBlock.local.nameExact("f").l
      localF.typeFullName shouldBe "ANY"
      val List(invokeCall) = cpg.call.nameExact("e?").l
      invokeCall.code shouldBe "e?()"
    }

    "testOptional5" in {
      val cpg = code("""
        |struct B<T> {}
        |var g = B<A?>()
        |""".stripMargin)
      val List(structB) = cpg.typeDecl.nameExact("B").l
      structB.fullName shouldBe "Test0.swift:<global>.B"
      val List(callB) = cpg.call.nameExact("B<A?>").l
      callB.code shouldBe "B<A?>()"
    }
  }

}
