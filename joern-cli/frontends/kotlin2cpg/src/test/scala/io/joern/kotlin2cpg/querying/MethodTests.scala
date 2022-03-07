package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple class declaration" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package com.test.pkg
        |
        |class Foo {
        |  fun bar(x: Int): Int {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should contain a method node for `bar` with the correct fields" in {
      val List(x) = cpg.method.name("bar").isExternal(false).l
      x.size shouldBe 1
      x.name shouldBe "bar"
      x.fullName shouldBe "com.test.pkg.Foo.bar:java.lang.Integer(java.lang.Integer)"
      x.code shouldBe "Int(x:Int)"
      x.signature shouldBe "java.lang.Integer(java.lang.Integer)"
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(4)
      x.columnNumber shouldBe Some(6)
      x.order shouldBe 1
      x.filename.endsWith(".kt") shouldBe true
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
