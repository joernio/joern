package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import java.io.File

class MultiModuleTests extends GoCodeToCpgSuite {
  "Module defined under another directory" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      Seq("module1", "go.mod").mkString(File.separator)
    ).moreCode(
      """
        |package fpkg
        |type Sample struct {
        |  Name string
        |}
        |func Woo(a int) int{
        |   return 0
        |}
        |""".stripMargin,
      Seq("module1", "lib", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib"
        |func main() {
        |  var a = fpkg.Woo(10)
        |  var b = fpkg.Sample{name: "Pandurang"}
        |  var c = b.Name
        |  var d fpkg.Sample
        |}
        |""".stripMargin,
      Seq("module1", "main.go").mkString(File.separator)
    )

    "Check METHOD Node" in {
      cpg.method("Woo").size shouldBe 1
      val List(x) = cpg.method("Woo").l
      x.fullName shouldBe "joern.io/sample/lib.Woo"
      x.signature shouldBe "joern.io/sample/lib.Woo(int)int"
    }

    "Check CALL Node" in {
      val List(x) = cpg.call("Woo").l
      x.methodFullName shouldBe "joern.io/sample/lib.Woo"
      x.typeFullName shouldBe "int"
    }

    "Traversal from call to callee method node" in {
      val List(x) = cpg.call("Woo").callee.l
      x.fullName shouldBe "joern.io/sample/lib.Woo"
      x.isExternal shouldBe false
    }

    "Check TypeDecl Node" in {
      val List(x) = cpg.typeDecl("Sample").l
      x.fullName shouldBe "joern.io/sample/lib.Sample"
    }

    "Check LOCAL Nodes" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "joern.io/sample/lib.Sample"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/sample/lib.Sample"
    }
  }

  "Multiple modules defined under one directory" should {
    val cpg = code(
      """
        |module joern.io/module1
        |go 1.18
        |""".stripMargin,
      Seq("module1", "go.mod").mkString(File.separator)
    ).moreCode(
      """
        |package pkg
        |type ModoneSample struct {
        |  Name string
        |}
        |func ModoneWoo(a int) int{
        |   return 0
        |}
        |""".stripMargin,
      Seq("module1", "pkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/module1/pkg"
        |func main() {
        |  var a = pkg.ModoneWoo(10)
        |  var b = pkg.ModoneSample{name: "Pandurang"}
        |  var c = b.Name
        |  var d pkg.ModoneSample
        |}
        |""".stripMargin,
      Seq("module1", "main.go").mkString(File.separator)
    ).moreCode(
      """
        |module joern.io/module2
        |go 1.18
        |""".stripMargin,
      Seq("module2", "go.mod").mkString(File.separator)
    ).moreCode(
      """
        |package pkg
        |type ModtwoSample struct {
        |  Name string
        |}
        |func ModtwoWoo(a int) int{
        |   return 0
        |}
        |""".stripMargin,
      Seq("module2", "pkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/module2/pkg"
        |func main() {
        |  var a = pkg.ModtwoWoo(10)
        |  var b = pkg.ModtwoSample{name: "Pandurang"}
        |  var c = b.Name
        |  var d pkg.ModtwoSample
        |}
        |""".stripMargin,
      Seq("module2", "main.go").mkString(File.separator)
    )

  }
}
