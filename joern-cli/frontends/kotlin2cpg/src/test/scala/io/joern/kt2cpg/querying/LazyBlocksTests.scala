package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import overflowdb.traversal.jIteratortoTraversal

class LazyBlocksTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple lazy blocks" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import java.nio.file.Files
        |
        |fun main() {
        |    val customDir = Files.createTempDirectory("custom").toFile()
        |    val foo = lazy { customDir }
        |    println(foo)
        |}
        |""".stripMargin)

    "should not contain any identifiers without an ast parent" in {
      cpg.identifier
        .filter(_._astIn.size == 0)
        .code
        .l shouldBe Seq()
    }
  }
}
