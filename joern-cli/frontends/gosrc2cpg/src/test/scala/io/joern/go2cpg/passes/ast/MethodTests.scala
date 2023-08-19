package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*

import java.io.File

class MethodTests extends GoCodeToCpgSuite {

  "Empty parameters" should {
    val cpg = code("""
        |package main
        |func foo() {
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo() {")
      x.signature shouldBe "main.foo ()"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "check binding Node" in {
      val List(x) = cpg.method.name("foo").bindingTypeDecl.l
      x.name shouldBe "main.<global>"
      x.fullName shouldBe "Test0.go:main.<global>"
    }
  }

  "Method arguments with primitive types" should {
    val cpg = code("""
                     |package main
                     |func foo(argc int, argv string) {
                     |}
                     |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv string)")
      x.signature shouldBe "main.foo (int, string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv string"
      argv.order shouldBe 2
      argv.typeFullName shouldBe "string"
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      argv.name shouldBe "argv"
    }
  }

  "More than one arguments of same Type" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv string) {
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc, arga int, argv string)")
      x.signature shouldBe "main.foo (int, int, string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }

    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "arga", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga int"
      arga.order shouldBe 2
      arga.typeFullName shouldBe "int"

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv string"
      argv.order shouldBe 3
      argv.typeFullName shouldBe "string"
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, arga, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      arga.name shouldBe "arga"
      argv.name shouldBe "argv"
    }
  }

  "Variable argument use case" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv ...string) {
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc, arga int, argv ...string)")
      x.signature shouldBe "main.foo (int, int, ...string)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "Test0.go:main.<global>"
      x.order shouldBe 1
      x.filename shouldBe "Test0.go"
      x.lineNumber shouldBe Option(3)
      x.lineNumberEnd shouldBe Option(4)
    }
    "be correct for parameter nodes" in {
      cpg.parameter.name.l shouldBe List("argc", "arga", "argv")
      val List(argc) = cpg.parameter.name("argc").l
      argc.code shouldBe "argc int"
      argc.order shouldBe 1
      argc.typeFullName shouldBe "int"
      argc.isVariadic shouldBe false

      val List(arga) = cpg.parameter.name("arga").l
      arga.code shouldBe "arga int"
      arga.order shouldBe 2
      arga.typeFullName shouldBe "int"
      arga.isVariadic shouldBe false

      val List(argv) = cpg.parameter.name("argv").l
      argv.code shouldBe "argv string"
      argv.order shouldBe 3
      argv.typeFullName shouldBe "string"
      argv.isVariadic shouldBe true
    }

    "traversing from parameter to method" in {
      cpg.parameter.name("argc").method.name.l shouldBe List("foo")
      cpg.parameter.name("arga").method.name.l shouldBe List("foo")
      cpg.parameter.name("argv").method.name.l shouldBe List("foo")
    }

    "traversing from method to parameter" in {
      val List(argc, arga, argv) = cpg.method.name("foo").parameter.l
      argc.name shouldBe "argc"
      arga.name shouldBe "arga"
      argv.name shouldBe "argv"
    }
  }

  "struct type argument from other package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      Seq("fpkg", "lib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo(argc int, argv fpkg.Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "should contain exactly one method node with correct fields" in {
      val List(x) = cpg.method.name("foo").l
      x.name shouldBe "foo"
      x.fullName shouldBe "main.foo"
      x.code should startWith("func foo(argc int, argv fpkg.Sample)")
      x.signature shouldBe "main.foo (int, fpkg.Sample)"
      x.isExternal shouldBe false
      x.astParentType shouldBe NodeTypes.TYPE_DECL
      x.astParentFullName shouldBe "main.go:main.<global>"
      x.order shouldBe 2
      x.filename shouldBe "main.go"
      x.lineNumber shouldBe Option(4)
      x.lineNumberEnd shouldBe Option(5)
    }
  }

  // TODO: Add unit tests for Array of primitives as well as struct.
  // TODO: Add unit test for tuple return
  // TODO: Add unit test with pointers
}
