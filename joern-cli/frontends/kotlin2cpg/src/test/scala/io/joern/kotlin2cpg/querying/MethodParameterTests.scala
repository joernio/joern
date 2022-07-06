package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class MethodParameterTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with a simple function definition" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun add1mul(x: Int, y: Int): Int {
        |  return (x + 1) * y
        |}
        |""".stripMargin)

    "should contain a METHOD with two METHOD_PARAMETER_IN nodes" in {
      cpg.parameter.filter(_.method.name == "add1mul").name.toSet shouldBe Set("x", "y")
    }

    "should contain METHOD_PARAMETER_IN nodes with the correct properties set" in {
      def params = cpg.parameter.filter(_.method.name == "add1mul")

      val List(x) = params.name("x").l
      x.code shouldBe "x"
      x.typeFullName shouldBe "int"
      x.lineNumber shouldBe Some(4)
      x.columnNumber shouldBe Some(12)
      x.order shouldBe 1

      val List(y) = params.name("y").l
      y.code shouldBe "y"
      y.typeFullName shouldBe "int"
      y.lineNumber shouldBe Some(4)
      y.columnNumber shouldBe Some(20)
      y.order shouldBe 2
    }
  }

  "CPG for code with a simple class definition" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |class Foo {
        |  fun bar(x: Int, y: Double) {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should return exactly two parameters with correct fields" in {
      def params = cpg.parameter.filter(_.method.name == "bar")
      params.name.toSet shouldBe Set("x", "y")

      val List(x) = params.name("x").l
      x.code shouldBe "x"
      x.typeFullName shouldBe "int"
      x.lineNumber shouldBe Some(5)
      x.columnNumber shouldBe Some(10)
      x.order shouldBe 1

      val List(y) = params.name("y").l
      y.code shouldBe "y"
      y.typeFullName shouldBe "double"
      y.lineNumber shouldBe Some(5)
      y.columnNumber shouldBe Some(18)
      y.order shouldBe 2
    }

    "should allow traversing from parameter to method" in {
      cpg.parameter.name("x").method.filter(_.isExternal == false).name.l shouldBe List("bar")
      cpg.parameter.name("y").method.filter(_.isExternal == false).name.l shouldBe List("bar")
    }
  }
}
