// This test file has been translated from swift/test/Parse/try.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TryTests extends AstSwiftSrc2CpgSuite {

  "TryTests" should {

    "testTry4" in {
      val cpg = code("""
        |var x = try foo() + bar()
        |x = try foo() + bar()
        |x += try foo() + bar()
        |x = foo() + try bar()
        |""".stripMargin)
      val List(try1, try2, try3, try4) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try2.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try3.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try4.astChildren.isCall.code.l shouldBe List("bar()")
    }

    "testTry5" in {
      val cpg = code("""
        |var y = true ? try foo() : try bar() + 0
        |""".stripMargin)
      val List(try1, try2) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo()")
      try2.astChildren.isCall.code.l shouldBe List("bar() + 0")
    }

    "testTry6" in {
      val cpg = code("""
        |var a = try! foo() + bar()
        |a = try! foo() + bar()
        |a += try! foo() + bar()
        |a = foo() + try! bar()
        |""".stripMargin)
      val List(try1, try2, try3, try4) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try2.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try3.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try4.astChildren.isCall.code.l shouldBe List("bar()")
    }

    "testTry7" in {
      val cpg = code("""
        |var b = true ? try! foo() : try! bar() + 0
        |""".stripMargin)
      val List(try1, try2) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo()")
      try2.astChildren.isCall.code.l shouldBe List("bar() + 0")
    }

    "testTry9" in {
      val cpg = code("""
        |var i = try? foo() + bar()
        |let _: Double = i
        |i = try? foo() + bar()
        |i += try? foo() + bar()
        |_ = foo() == try? bar()
        |_ = (try? foo()) == bar()
        |_ = foo() == (try? bar())
        |_ = (try? foo()) == (try? bar())
        |""".stripMargin)
      val List(try1, try2, try3, try4, try5, try6, try7, try8) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try2.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try3.astChildren.isCall.code.l shouldBe List("foo() + bar()")
      try4.astChildren.isCall.code.l shouldBe List("bar()")
      try5.astChildren.isCall.code.l shouldBe List("foo()")
      try6.astChildren.isCall.code.l shouldBe List("bar()")
      try7.astChildren.isCall.code.l shouldBe List("foo()")
      try8.astChildren.isCall.code.l shouldBe List("bar()")
    }

    "testTry10" in {
      val cpg = code("""
        |let j = true ? try? foo() : try? bar() + 0
        |""".stripMargin)
      val List(try1, try2) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo()")
      try2.astChildren.isCall.code.l shouldBe List("bar() + 0")
    }

    "testTry18" in {
      val cpg        = code("let _ = (try? foo())!!")
      val List(try1) = cpg.controlStructure.isTry.l
      try1.astChildren.isCall.code.l shouldBe List("foo()")
    }

    "testTry32" in {
      val cpg = code("""
        |let _: Int? = try? produceAny() as? Int
        |let _: Int?? = (try? produceAny()) as? Int // good
        |let _: String = try? produceAny() as? Int
        |let _: String = (try? produceAny()) as? Int
        |""".stripMargin)
      cpg.controlStructure.isTry.astChildren.isCall.code.distinct.l shouldBe List(
        "produceAny() as? Int",
        "produceAny()"
      )
    }

  }

}
