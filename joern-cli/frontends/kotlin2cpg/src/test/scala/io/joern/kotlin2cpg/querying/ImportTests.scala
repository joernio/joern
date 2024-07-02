package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ImportTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with a stdlib import" should {
    val cpg = code("""
        |package mypkg
        |
        |import kotlin.io.collections.listOf
        |import kotlin.comparisons.*
        |
        |fun main(args : Array<String>) {
        |  println("Hello, world")
        |}
        |""".stripMargin)

    "should contain the correct number of IMPORT nodes" in {
      cpg.imports.size should not be 0
    }

    "should contain IMPORT node for `listOf` entry with the correct properties set" in {
      val List(imp) = cpg.imports.code(".*listOf.*").l
      imp.code shouldBe "import kotlin.io.collections.listOf"
      imp.importedEntity shouldBe Some("kotlin.io.collections.listOf")
      imp.lineNumber shouldBe Some(4)
      imp.columnNumber shouldBe Some(0)
      imp.isWildcard shouldBe Some(false)
      imp.isExplicit shouldBe Some(true)
    }

    "should contain IMPORT node for wildcard `comparisons` entry with the correct properties set" in {
      val List(imp) = cpg.imports.code(".*comparisons.*").l
      imp.code shouldBe "import kotlin.comparisons.*"
      imp.importedEntity shouldBe Some("kotlin.comparisons.*")
      imp.lineNumber shouldBe Some(5)
      imp.columnNumber shouldBe Some(0)
      imp.isWildcard shouldBe Some(true)
      imp.isExplicit shouldBe Some(true)
    }
  }

  "CPG for code without explicit imports" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main(args : Array<String>) {
        |  println("Hello, world")
        |}
        |""".stripMargin)

    "should not contain any IMPORT nodes" in {
      cpg.imports.size shouldBe 0
    }
  }
}
