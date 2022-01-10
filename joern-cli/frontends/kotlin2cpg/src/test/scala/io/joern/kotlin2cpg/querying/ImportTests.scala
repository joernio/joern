package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ImportTests extends AnyFreeSpec with Matchers {

  "CPG for code with a stdlib import" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      cpg.all.collect { case n: Import => n }.size should not be 0
    }

    "should contain IMPORT node for `listOf` entry with the correct properties set" in {
      val List(imp) = cpg.all.collect { case n: Import => n }.code(".*listOf.*").l
      imp.code shouldBe "import kotlin.io.collections.listOf"
      imp.importedEntity shouldBe Some("kotlin.io.collections.listOf")
      imp.lineNumber shouldBe Some(3)
      imp.columnNumber shouldBe Some(0)
      imp.isWildcard shouldBe Some(false)
      imp.isExplicit shouldBe Some(true)
    }

    "should contain IMPORT node for wildcard `comparisons` entry with the correct properties set" in {
      val List(imp) = cpg.all.collect { case n: Import => n }.code(".*comparisons.*").l
      imp.code shouldBe "import kotlin.comparisons.*"
      imp.importedEntity shouldBe Some("kotlin.comparisons.*")
      imp.lineNumber shouldBe Some(4)
      imp.columnNumber shouldBe Some(0)
      imp.isWildcard shouldBe Some(true)
      imp.isExplicit shouldBe Some(true)
    }
  }

  "CPG for code without explicit imports" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args : Array<String>) {
        |  println("Hello, world")
        |}
        |""".stripMargin)

    "should not contain any IMPORT nodes" in {
      cpg.all.collect { case n: Import => n }.size shouldBe 0
    }
  }
}
