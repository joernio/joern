package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class ImportTests extends GoCodeToCpgSuite {

  "imports which follow direct style import" should {
    val cpg = code("""
        |package main
        |import "math"
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 1
      imports.importedAs.l shouldBe List("math")
      imports.importedEntity.l shouldBe List("math")
      imports.lineNumber.l shouldBe List(3)
    }
  }

  "imports which follow grouped style import" should {
    val cpg = code("""
        |package main
        |import(
        |    "fmt"
        |    "math"
        |)
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 2
      imports.importedAs.l shouldBe List("fmt", "math")
      imports.importedEntity.l shouldBe List("fmt", "math")
    }
  }

  "imports which follow nested style import" should {
    val cpg = code("""
        |package main
        |import "math/rand"
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 1
      imports.importedEntity.l shouldBe List("math/rand")
      imports.importedAs.l shouldBe List("rand")
    }
  }

  "imports which follow alaised style import" should {
    val cpg = code("""
        |package main
        |import m "math"
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 1
      imports.importedAs.l shouldBe List("m")
      imports.importedEntity.l shouldBe List("math")
    }
  }

  "imports which follow dot style import" should {
    val cpg = code("""
        |package main
        |import . "math"
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 1
      imports.importedAs.l shouldBe List(".")
      imports.importedEntity.l shouldBe List("math")
    }
  }

  "external package import sample" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func main() {}
        |""".stripMargin)
    "have import node created" in {
      val imports = cpg.imports.l
      imports.size shouldBe 1
      imports.importedAs.l shouldBe List("fpkg")
      imports.importedEntity.l shouldBe List("joern.io/sample/fpkg")
    }
  }
}
