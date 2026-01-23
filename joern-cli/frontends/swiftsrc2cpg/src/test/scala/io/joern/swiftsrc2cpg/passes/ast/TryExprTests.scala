// This test file has been translated from swift/test/Parse/try.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.semanticcpg.language.*

class TryExprTests extends AstSwiftSrc2CpgSuite {

  "TryExprTests" should {

    "testTryExpr4" in {
      val cpg = code("""
        |var x = try foo() + bar()
        |x = try foo() + bar()
        |x += try foo() + bar()
        |x = foo() + try bar()
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "var x = try foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "x = try foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "x += try foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "x = foo() + try bar()",
        "foo() + try bar()",
        "foo()",
        "bar()"
      )
    }

    "testTryExpr5" in {
      val cpg = code("""
        |var y = true ? try foo() : try bar() + 0
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "var y = true ? try foo() : try bar() + 0",
        "true ? try foo() : try bar() + 0",
        "foo()",
        "bar() + 0",
        "bar()"
      )
    }

    "testTryExpr6" in {
      val cpg = code("""
        |var a = try! foo() + bar()
        |a = try! foo() + bar()
        |a += try! foo() + bar()
        |a = foo() + try! bar()
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "var a = try! foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "a = try! foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "a += try! foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "a = foo() + try! bar()",
        "foo() + try! bar()",
        "foo()",
        "bar()"
      )
    }

    "testTryExpr7" in {
      val cpg = code("""
        |var b = true ? try! foo() : try! bar() + 0
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "var b = true ? try! foo() : try! bar() + 0",
        "true ? try! foo() : try! bar() + 0",
        "foo()",
        "bar() + 0",
        "bar()"
      )
    }

    "testTryExpr9" in {
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
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "var i = try? foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "let <wildcard>0: Double = i",
        "i = try? foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "i += try? foo() + bar()",
        "foo() + bar()",
        "foo()",
        "bar()",
        "_ = foo() == try? bar()",
        "foo() == try? bar()",
        "foo()",
        "bar()",
        "_ = (try? foo()) == bar()",
        "(try? foo()) == bar()",
        "foo()",
        "bar()",
        "_ = foo() == (try? bar())",
        "foo() == (try? bar())",
        "foo()",
        "bar()",
        "_ = (try? foo()) == (try? bar())",
        "(try? foo()) == (try? bar())",
        "foo()",
        "bar()"
      )
    }

    "testTryExpr10" in {
      val cpg = code("""
        |let j = true ? try? foo() : try? bar() + 0
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "let j = true ? try? foo() : try? bar() + 0",
        "true ? try? foo() : try? bar() + 0",
        "foo()",
        "bar() + 0",
        "bar()"
      )
    }

    "testTryExpr18" in {
      val cpg = code("let _ = (try? foo())!!")
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List("let <wildcard>0 = (try? foo())!!", "foo()")
    }

    "testTryExpr32" in {
      val cpg = code("""
        |let _: Int? = try? produceAny() as? Int
        |let _: Int?? = (try? produceAny()) as? Int
        |let _: String = try? produceAny() as? Int
        |let _: String = (try? produceAny()) as? Int
        |""".stripMargin)
      cpg.controlStructure.isTry shouldBe empty
      cpg.call.code.l shouldBe List(
        "let <wildcard>0: Int? = try? produceAny() as? Int",
        "produceAny() as? Int",
        "produceAny()",
        "let <wildcard>1: Int?? = (try? produceAny()) as? Int",
        "(try? produceAny()) as? Int",
        "produceAny()",
        "let <wildcard>2: String = try? produceAny() as? Int",
        "produceAny() as? Int",
        "produceAny()",
        "let <wildcard>3: String = (try? produceAny()) as? Int",
        "(try? produceAny()) as? Int",
        "produceAny()"
      )
    }

  }

}
