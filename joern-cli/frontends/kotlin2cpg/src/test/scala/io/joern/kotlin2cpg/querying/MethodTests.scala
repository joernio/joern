package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class MethodTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple method defined at package-level" should {
    lazy val cpg = code("""
       |fun double(x: Int): Int {
       |  return x * 2
       |}
       |
       |fun main(args : Array<String>) {
       |  println("The double of 2 is: " + double(2))
       |}
       |""".stripMargin)

    "should contain exactly two non-external methods" in {
      cpg.method.isExternal(false).size shouldBe 2
    }

    "should contain method nodes with the correct fields" in {
      val List(x) = cpg.method.name("double").isExternal(false).l
      x.size shouldBe 1
      x.fullName shouldBe "double:int(int)"
      x.code shouldBe "double"
      x.signature shouldBe "int(int)"
      x.isExternal shouldBe false
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(4)
      x.filename.endsWith(".kt") shouldBe true

      val List(y) = cpg.method.name("main").isExternal(false).l
      y.size shouldBe 1
      y.fullName shouldBe "main:void(kotlin.Array)"
      y.code shouldBe "main"
      y.signature shouldBe "void(kotlin.Array)"
      y.isExternal shouldBe false
      y.lineNumber shouldBe Some(6)
      x.columnNumber shouldBe Some(4)
      y.filename.endsWith(".kt") shouldBe true
    }

    "should allow traversing to parameters" in {
      cpg.method.name("double").isExternal(false).parameter.name.toSet shouldBe Set("x")
      cpg.method.name("main").isExternal(false).parameter.name.toSet shouldBe Set("args")
    }

    "should allow traversing to methodReturn" in {
      cpg.method.name("double").isExternal(false).methodReturn.typeFullName.l shouldBe List("int")
      cpg.method.name("main").isExternal(false).methodReturn.typeFullName.l shouldBe List("void")
    }

    "should allow traversing to file" in {
      cpg.method.name("double").isExternal(false).file.name.l should not be empty
      cpg.method.name("main").isExternal(false).file.name.l should not be empty
    }

    "should allow traversing to block" in {
      cpg.method.name("double").isExternal(false).block.l should not be empty
      cpg.method.name("main").isExternal(false).block.l should not be empty
    }
  }

  "CPG for code with simple class declaration" should {
    lazy val cpg = code("""
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
