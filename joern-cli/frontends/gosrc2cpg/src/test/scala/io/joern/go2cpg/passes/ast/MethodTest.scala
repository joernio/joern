package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodTest extends GoCodeToCpgSuite {

  "Method Test 1" should {
    val cpg = code("""
        |package main
        |func foo(argc int, argv string) int {
        |	return 0
        |}
        |""".stripMargin)

    "should contain exactly one method node with correct fields" in {
      inside(cpg.method.name("foo").l) { case List(x) =>
        x.name shouldBe "foo"
        x.fullName shouldBe "main.foo"
//        x.code should startWith("func foo(int argc, char **argv) {")
//        x.signature shouldBe "foo (int,char**) int"
        x.isExternal shouldBe false
        x.order shouldBe 1
        x.filename shouldBe "Test0.go"
//        x.lineNumber shouldBe Option(3)
//        x.lineNumberEnd shouldBe Option(4)
//        x.columnNumber shouldBe Option(1)
//        x.columnNumberEnd shouldBe Option(2)
      }
    }

  }

}
