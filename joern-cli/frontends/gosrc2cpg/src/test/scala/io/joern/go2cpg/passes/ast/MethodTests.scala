package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
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
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
        x.code should startWith("func foo() {")
        x.signature shouldBe "main.foo ()"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.go"
        x.lineNumber shouldBe Option(3)
        x.lineNumberEnd shouldBe Option(4)
      }
    }
  }

  "Method Test 1" should {
    val cpg = code("""
                     |package main
                     |func foo(argc int, argv string) {
                     |}
                     |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
        x.code should startWith("func foo(argc int, argv string)")
        x.signature shouldBe "main.foo (int, string)"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.go"
        x.lineNumber shouldBe Option(3)
        x.lineNumberEnd shouldBe Option(4)
      }
    }
  }

  "Method Test 2" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv string) {
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
        x.code should startWith("func foo(argc, arga int, argv string)")
        x.signature shouldBe "main.foo (int, int, string)"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.go"
        x.lineNumber shouldBe Option(3)
        x.lineNumberEnd shouldBe Option(4)
      }
    }
  }

  "Method Test 3" should {
    val cpg = code("""
        |package main
        |func foo(argc, arga int, argv ...string) {
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
        x.code should startWith("func foo(argc, arga int, argv ...string)")
        x.signature shouldBe "main.foo (int, int, ...string)"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.go"
        x.lineNumber shouldBe Option(3)
        x.lineNumberEnd shouldBe Option(4)
      }
    }
  }

  "Method Test 4" should {
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
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
        x.code should startWith("func foo(argc int, argv fpkg.Sample)")
        x.signature shouldBe "main.foo (int, fpkg.Sample)"
        x.isExternal shouldBe false
        x.order shouldBe 2
        x.filename shouldBe "main.go"
        x.lineNumber shouldBe Option(4)
        x.lineNumberEnd shouldBe Option(5)
      }
    }
  }

}
