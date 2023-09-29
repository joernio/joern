package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

import java.io.File

class GlobalVariableAndConstantTests extends GoCodeToCpgSuite {

  "Global variable declaration check" should {
    val cpg = code("""
        |package main
        |const (
        |	FooConst = "Test"
        |)
        |var (
        |	BarVar = 100
        |)
        |func main() {
        |  println(FooConst)
        |}
        |""".stripMargin)

    "Check LOCAL node" in {
      val List(a, b) = cpg.local.l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "int"
    }
  }

  "Var defined(with type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |var(
        |	SchemeHTTP string =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x) = cpg.call(Operators.fieldAccess).l
      x.typeFullName shouldBe "string"
    }

    "Check methodfullname of variable imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }

  "Var defined(without type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |var(
        |	SchemeHTTP =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x) = cpg.call(Operators.fieldAccess).l
      x.typeFullName shouldBe "string"
    }

    "Check methodfullname of variable imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }
  "Const defined(with type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |const (
        |	SchemeHTTP string =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(x) = cpg.call(Operators.fieldAccess).l
      x.typeFullName shouldBe "string"
    }

    "Check methodfullname of constant imported from other package " in {
      val List(callNode) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
    }
  }

  "const defined(without type mentioned) in one package used in another package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib1
        |
        |const (
        |	SchemeHTTP =  "http"
        |)
        |
        |""".stripMargin,
      Seq("lib1", "typelib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib1"
        |import "joern.io/sample/lib2"
        |func main() {
        |	var a = lib1.SchemeHTTP.value()
        |   var b = lib2.SchemeHTTP.value()
        |}
        |""".stripMargin,
      "main.go"
    )

    "Be correct for Field Access CALL Node for Global variable access" in {
      val List(a, b) = cpg.call(Operators.fieldAccess).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "joern.io/sample/lib2.SchemeHTTP.<FieldAccess>.<unknown>"
    }

    "Check methodfullname of constant imported from other package " in {
      val List(callNode, callNode2) = cpg.call("value").l
      callNode.methodFullName shouldBe "string.value"
      callNode2.methodFullName shouldBe "joern.io/sample/lib2.SchemeHTTP.<FieldAccess>.<unknown>.value"
    }
  }
}
