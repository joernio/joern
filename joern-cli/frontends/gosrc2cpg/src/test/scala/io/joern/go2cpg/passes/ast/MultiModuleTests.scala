package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import java.io.File
import scala.collection.immutable.List

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
    "Check METHOD Node module 1" in {
      cpg.method("ModoneWoo").size shouldBe 1
      val List(x) = cpg.method("ModoneWoo").l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.signature shouldBe "joern.io/module1/pkg.ModoneWoo(int)int"
    }

    "Check METHOD Node module 2" in {
      cpg.method("ModtwoWoo").size shouldBe 1
      val List(x) = cpg.method("ModtwoWoo").l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.signature shouldBe "joern.io/module2/pkg.ModtwoWoo(int)int"
    }

    "Check CALL Node module 1" in {
      val List(x) = cpg.call("ModoneWoo").l
      x.methodFullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.typeFullName shouldBe "int"
    }

    "Check CALL Node module 2" in {
      val List(x) = cpg.call("ModtwoWoo").l
      x.methodFullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.typeFullName shouldBe "int"
    }

    "Traversal from call to callee method node module 1" in {
      val List(x) = cpg.call("ModoneWoo").callee.l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.isExternal shouldBe false
    }

    "Traversal from call to callee method node module 2" in {
      val List(x) = cpg.call("ModtwoWoo").callee.l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.isExternal shouldBe false
    }

    "Check TypeDecl Node module 1" in {
      val List(x) = cpg.typeDecl("ModoneSample").l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneSample"
    }

    "Check TypeDecl Node module 2" in {
      val List(x) = cpg.typeDecl("ModtwoSample").l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoSample"
    }

    "Check LOCAL Nodes Module 1 and 2" in {
      val List(a, b, c, d, e, f, g, h) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "joern.io/module1/pkg.ModoneSample"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/module1/pkg.ModoneSample"

      e.typeFullName shouldBe "int"
      f.typeFullName shouldBe "joern.io/module2/pkg.ModtwoSample"
      g.typeFullName shouldBe "string"
      h.typeFullName shouldBe "joern.io/module2/pkg.ModtwoSample"
    }
  }

  "Multiple modules defined one inside another" should {
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
      Seq("module1", "stage", "src", "module2", "go.mod").mkString(File.separator)
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
      Seq("module1", "stage", "src", "module2", "pkg", "lib.go").mkString(File.separator)
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
      Seq("module1", "stage", "src", "module2", "main.go").mkString(File.separator)
    )
    "Check METHOD Node module 1" in {
      cpg.method("ModoneWoo").size shouldBe 1
      val List(x) = cpg.method("ModoneWoo").l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.signature shouldBe "joern.io/module1/pkg.ModoneWoo(int)int"
    }

    "Check METHOD Node module 2" in {
      cpg.method("ModtwoWoo").size shouldBe 1
      val List(x) = cpg.method("ModtwoWoo").l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.signature shouldBe "joern.io/module2/pkg.ModtwoWoo(int)int"
    }

    "Check CALL Node module 1" in {
      val List(x) = cpg.call("ModoneWoo").l
      x.methodFullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.typeFullName shouldBe "int"
    }

    "Check CALL Node module 2" in {
      val List(x) = cpg.call("ModtwoWoo").l
      x.methodFullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.typeFullName shouldBe "int"
    }

    "Traversal from call to callee method node module 1" in {
      val List(x) = cpg.call("ModoneWoo").callee.l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneWoo"
      x.isExternal shouldBe false
    }

    "Traversal from call to callee method node module 2" in {
      val List(x) = cpg.call("ModtwoWoo").callee.l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoWoo"
      x.isExternal shouldBe false
    }

    "Check TypeDecl Node module 1" in {
      val List(x) = cpg.typeDecl("ModoneSample").l
      x.fullName shouldBe "joern.io/module1/pkg.ModoneSample"
    }

    "Check TypeDecl Node module 2" in {
      val List(x) = cpg.typeDecl("ModtwoSample").l
      x.fullName shouldBe "joern.io/module2/pkg.ModtwoSample"
    }

    "Check LOCAL Nodes Module 1 and 2" in {
      val List(a, b, c, d, e, f, g, h) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "joern.io/module2/pkg.ModtwoSample"
      c.typeFullName shouldBe "string"
      d.typeFullName shouldBe "joern.io/module2/pkg.ModtwoSample"

      e.typeFullName shouldBe "int"
      f.typeFullName shouldBe "joern.io/module1/pkg.ModoneSample"
      g.typeFullName shouldBe "string"
      h.typeFullName shouldBe "joern.io/module1/pkg.ModoneSample"
    }
  }
}
