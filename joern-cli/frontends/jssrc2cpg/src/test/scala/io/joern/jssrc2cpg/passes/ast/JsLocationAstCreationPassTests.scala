package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class JsLocationAstCreationPassTests extends JsSrc2CpgSuite {

  "AST generation for line and column numbers" should {

    "have the position of the parser for a function" in {
      val cpg = code("""
        |function f() {
        |  return 1;
        |}""".stripMargin)
      val List(f) = cpg.method.nameExact("f").l
      f.lineNumber shouldBe Option(2)
      f.columnNumber shouldBe Option(0)
      f.lineNumberEnd shouldBe Option(4)
      f.columnNumberEnd shouldBe Option(1)
    }

    "have the position of the parser for a file with CRLF line endings" in {
      val cpg     = code("var a = 1;\r\nfunction f() {\r\n  return 1;\r\n}")
      val List(f) = cpg.method.nameExact("f").l
      f.lineNumber shouldBe Option(2)
      f.columnNumber shouldBe Option(0)
      f.lineNumberEnd shouldBe Option(4)
      f.columnNumberEnd shouldBe Option(1)
    }

    "end a line like the parser does: at a lone carriage return" in {
      val cpg     = code("var a = 1;\rfunction f() {\r  return 1;\r}")
      val List(f) = cpg.method.nameExact("f").l
      f.lineNumber shouldBe Option(2)
      f.columnNumber shouldBe Option(0)
      f.lineNumberEnd shouldBe Option(4)
      f.columnNumberEnd shouldBe Option(1)
    }

    "end a line like the parser does: at the unicode line separator" in {
      val cpg     = code("var a = 1;\u2028function f() {\u2029  return 1;\u2028}")
      val List(f) = cpg.method.nameExact("f").l
      f.lineNumber shouldBe Option(2)
      f.columnNumber shouldBe Option(0)
      f.lineNumberEnd shouldBe Option(4)
      f.columnNumberEnd shouldBe Option(1)
    }

    "locate the constructor of a class without one at the class" in {
      val cpg = code("""
        |var x = 1;
        |class ClassA {
        |  a = 1
        |}""".stripMargin)
      val List(constructor) = cpg.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l
      constructor.lineNumber shouldBe Option(3)
      constructor.columnNumber shouldBe Option(0)
      constructor.lineNumberEnd shouldBe Option(5)
      constructor.columnNumberEnd shouldBe Option(1)
    }

  }

}
