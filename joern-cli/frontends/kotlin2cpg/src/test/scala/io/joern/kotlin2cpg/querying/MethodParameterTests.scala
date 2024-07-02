package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language.*

class MethodParameterTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with a simple function declaration" should {
    val cpg = code("""
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

  "CPG for code with a simple class declaration" should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo {
        |  fun bar(x: Int, y: Double) {
        |    return x * 2
        |  }
        |}
        |""".stripMargin)

    "should contain two METHOD_PARAMETER_IN nodes with the correct props set" in {
      val List(firstParam: MethodParameterIn, secondParam: MethodParameterIn, thirdParam: MethodParameterIn) =
        cpg.method.name("bar").parameter.l

      firstParam.code shouldBe "this"
      firstParam.typeFullName shouldBe "mypkg.Foo"
      firstParam.order shouldBe 0

      secondParam.code shouldBe "x"
      secondParam.typeFullName shouldBe "int"
      secondParam.lineNumber shouldBe Some(5)
      secondParam.columnNumber shouldBe Some(10)
      secondParam.order shouldBe 1

      thirdParam.code shouldBe "y"
      thirdParam.typeFullName shouldBe "double"
      thirdParam.lineNumber shouldBe Some(5)
      thirdParam.columnNumber shouldBe Some(18)
      thirdParam.order shouldBe 2
    }

    "should allow traversing from parameter to method" in {
      cpg.parameter.name("x").method.filter(_.isExternal == false).name.l shouldBe List("bar")
      cpg.parameter.name("y").method.filter(_.isExternal == false).name.l shouldBe List("bar")
    }
  }
}
