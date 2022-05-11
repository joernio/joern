package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple class declaration" - {
    lazy val cpg = TestContext.buildCpg("""
        |package com.test.pkg
        |
        |class Foo {
        |  fun bar(x: Int): Int {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should contain a METHOD node for `bar` with the props set" in {
      val List(m) = cpg.method.name("bar").l
      m.size shouldBe 1
      m.name shouldBe "bar"
      m.fullName shouldBe "com.test.pkg.Foo.bar:int(int)"
      m.code shouldBe "bar"
      m.signature shouldBe "int(int)"
      m.isExternal shouldBe false
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(6)
      m.lineNumberEnd shouldBe Some(7)
      m.columnNumberEnd shouldBe Some(2)
      m.order shouldBe 1
      m.filename.endsWith(".kt") shouldBe true
    }

    "should allow traversing to parameters" in {
      cpg.method.name("bar").parameter.name.toSet shouldBe Set("x")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("bar").methodReturn.l.size shouldBe 1
    }

    "should allow traversing to file" in {
      cpg.method.name("bar").file.name.l should not be empty
    }

    "should allow traversing to block" in {
      cpg.method.name("bar").block.l should not be empty
    }
  }
}
